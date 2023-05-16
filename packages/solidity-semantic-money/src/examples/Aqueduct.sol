// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;


import {
    Time, FlowRate, Value, Unit
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    FlowId, ToySuperfluidToken
} from "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperfluidToken.sol";
import {
    ToySuperfluidPool
} from "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperfluidPool.sol";

import {IConstantFlowAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import {ISuperfluid} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import "./sqrtLib.sol";


/**
 * @dev Aqueduct - a (maybe) zero-intermediate liquidity market maker (mZILMM)
 */
contract Aqueduct {

    // Constants
    FlowId constant public SWAP_DISTRIBUTE_FLOW_ID = FlowId.wrap(0);
    FlowId constant public ADJUSTMENT_FLOW_ID = FlowId.wrap(0);
    bytes32 public constant CFA_ID = keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    uint256 internal constant FLOW_PER_UNIT = 1e9;
    uint256 constant public e16 = 65536; // 2^16
    uint256 constant public e32 = 4294967296; // 2^32

    // Contract state    
    ISuperfluid immutable host;
    IConstantFlowAgreementV1 immutable cfa;
    ToySuperfluidToken immutable tokenA;
    ToySuperfluidToken immutable tokenB;

    // The feeRecipient should be sent all remainder flow rate,
    // which includes protocol fees at around 0.01% (initally) of total streamed through protocol,
    // so the fee recipient should be an address controlled by the protocol
    address feeRecipient;

    struct PoolParameters {
        uint16 F;
        uint16 P;
        uint16 M;
        uint16 N;
    }
    PoolParameters params;

    struct PoolTotals {
        uint256 A;
        uint256 B;
        uint256 C;
        uint256 D;
        uint256 H;
    }
    PoolTotals totals;

    struct PoolFlows {
        uint256 XA;
        uint256 XB;
        uint256 XC;
        uint256 YA;
        uint256 YB;
        uint256 YC;
    }
    PoolFlows flows;

    // Combining all three incoming (a, b, c) and two outgoing (x, y) flows
    // gives six pools in total, named XA, XB, XC, YA, YB, YC
    // x, y are user outflows of tokens A, B respectively
    // a, b are user inflows of tokens A, B respectively
    // c is user inflow of liquidity = sqrt(a*b)
    struct Pools {
        ToySuperfluidPool XA;
        ToySuperfluidPool XB;
        ToySuperfluidPool XC;
        ToySuperfluidPool YA;
        ToySuperfluidPool YB;
        ToySuperfluidPool YC;
    }
    Pools pools;

    constructor (ISuperfluid _host, ToySuperfluidToken _tokenA, ToySuperfluidToken _tokenB, address _feeRecipient) {
        // Set contract state
        host = _host;
        cfa = IConstantFlowAgreementV1(address(host.getAgreementClass(CFA_ID)));
        tokenA = _tokenA;
        tokenB = _tokenB;
        feeRecipient = _feeRecipient;

        // Set initial values for PoolParameters
        params.F = 328; // 328/65536 = 0.0049896, so trading fee initially approx 0.5%
        params.P = 1311; // 1310/65536 = 0.019989, so protocol fee is around 2% of rewards distributed
        params.M = 10; // M/N = 10, so liquidity providers get around 10x the rewards as traders
        params.N = 1; // only ratio matters, so M=100 and N=5 has same outcome as M=20 N=1

        // PoolTotals are all zero at initialisation, no user is yet streaming tokens in

        // PoolFlows are also all zero at initialisation

        // Create Pools
        pools.XA = tokenA.createPool();
        pools.XB = tokenA.createPool();
        pools.XC = tokenA.createPool();
        pools.YA = tokenB.createPool();
        pools.YB = tokenB.createPool();
        pools.YC = tokenB.createPool();
    }

    /**
     * @dev Flow updated callback
     * @params token - token for the flow
     * @params from - flow sender
     * @params ir0 - previous (input) flow rate
     * @params ir1 - new (input) flow rate
     */
    function onFlowUpdate(ToySuperfluidToken token, address from, FlowRate ir0, FlowRate ir1) external {

        // TODO: how large do incoming flow rates have to be for flows.XA (etc) to overflow?
        // There are some calcs there which are 3 uint256s multiplied together
        // Ought to prevent incoming flow rates from being too big

        // TODO: should Safecast be used throughout these methods?

        uint256 r0 = uint256(FlowRate.unwrap(ir0));
        uint256 r1 = uint256(FlowRate.unwrap(ir1));
        if (token == tokenA) {
            (, int96 _r, , ) = cfa.getFlow(tokenB, from, address(this));
            uint256 r = uint256(_r);
            _onFlowUpdate(from, r0, r1, r, r);

        } else if (token == tokenB) {
            (, int96 _r, , ) = cfa.getFlow(tokenA, from, address(this));
            uint256 r = uint256(_r);
            _onFlowUpdate(from, r, r, r0, r1);

        } else revert("Unknown token");
    }

    /**
     * @dev Inner function to handle updated flows
     * @params user - flow sender
     * @params a0 - previous inbound flow rate of token A
     * @params a1 - updated inbound flow rate of token A
     * @params b0 - previous inbound flow rate of token B
     * @params b1 - updated inbound flow rate of token B
     */
    function _onFlowUpdate(address user, uint256 a0, uint256 a1, uint256 b0, uint256 b1) internal {

        // User incoming flow rates a, b are known, of tokens A, B respectively
        // Calculate user flow rate c of liquidity, requires square root
        uint256 c0 = sqrtLib.sqrt(a0 * b0);
        uint256 c1 = sqrtLib.sqrt(a1 * b1);

        // Calculate updated PoolTotals
        totals.A = totals.A - a0 + a1; // pool inflow of tokenA
        totals.B = totals.B - b0 + b1; // pool inflow of tokenB
        totals.C = totals.C - c0 + c1; // pool inflow of liquidity
        totals.H = sqrtLib.sqrt(totals.A * totals.B); // Useful intermediate calc; square root of sums != sum of square roots
        totals.D = uint256(params.M) * totals.H * totals.C + 2 * uint256(params.N) * totals.A * totals.B; // pool inflow of user reward factors

        // Calculate updated PoolFlows, there are two regimes
        // Pool Total H > 0 is operating regime where swaps occur
        // Pool Total H = 0 is returns regime where all funds are returned to the user
        if (totals.H > 0) {
            // Operating regime. Tokens A, B both being streamed in. Do swaps.
            // Intermediate calculations u, v
            uint256 u = (e16 - uint256(params.F)) * e16; // (1 - Trading Fee) * 2^32
            uint256 v = (e16 - uint256(params.P)) * uint256(params.F); // Rewards * 2^32 = Trading Fee * (1 - Protocol Fee) * 2^32
            // Swaps + other token rewards
            flows.XB = (u * totals.A) / e32 + (v * uint256(params.N) * totals.A * totals.A * totals.B) / (e32 * totals.D);
            flows.YA = (u * totals.B) / e32 + (v * uint256(params.N) * totals.A * totals.B * totals.B) / (e32 * totals.D);
            // Liquidity rewards
            flows.XC = (v * uint256(params.M) * totals.A * totals.C * totals.H) / (e32 * totals.D);
            flows.YC = (v * uint256(params.M) * totals.B * totals.C * totals.H) / (e32 * totals.D);
            // Same token rewards
            flows.XA = (v * uint256(params.N) * totals.A * totals.A * totals.B) / (e32 * totals.D);
            flows.YB = (v * uint256(params.N) * totals.A * totals.B * totals.B) / (e32 * totals.D);
        } else {
            // Returns regime. Either token A or token B (or both) are not being streamed in. Return all funds to user.
            // Zero swaps
            flows.XB = 0;
            flows.YA = 0;
            // Zero liquidity rewards
            flows.XC = 0;
            flows.YC = 0;
            // Return same token to users
            flows.XA = totals.A;
            flows.YB = totals.B;
        }

        // // AqueductLibrary.TraderSideState memory tssB;
        // FlowRate drr; // distribution rate requested
        // (a.state, tssB, drr) = AqueductLibrary.updateSide(a.state, ir0, ir1);

        {
            // FlowRate dr0 = a.pool.getDistributionFlowRate();

            // TODO: need six separate SWAP_DISTRIBUTE_FLOW_ID values here? 

            tokenA.distributeFlow(address(this), pools.XA, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.XA))));
            tokenA.distributeFlow(address(this), pools.XB, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.XB))));
            tokenA.distributeFlow(address(this), pools.XC, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.XC))));
            tokenB.distributeFlow(address(this), pools.YA, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.YA))));
            tokenB.distributeFlow(address(this), pools.YB, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.YB))));
            tokenB.distributeFlow(address(this), pools.YC, SWAP_DISTRIBUTE_FLOW_ID, FlowRate.wrap(int128(uint128(flows.YC))));

            // _adjustFlowRemainder(a, ir1 - ir0, dr1 - dr0);
        }
        {
            // // cancel existing remainder flow
            // FlowRate ar0 = b.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
            // FlowRate dr0 = b.pool.getDistributionFlowRate();

            // Set new units, these are scaled versions of updated flow rates a, b, c
            int128 unitsA = int128(uint128(a1 / FLOW_PER_UNIT));
            int128 unitsB = int128(uint128(b1 / FLOW_PER_UNIT));
            int128 unitsC = int128(uint128(c1 / FLOW_PER_UNIT));

            // TODO: use Safecast?
            // Update members with new units
            pools.XA.updateMember(from, Units.wrap(unitsA));
            pools.XB.updateMember(from, Units.wrap(unitsB));
            pools.XC.updateMember(from, Units.wrap(unitsC));
            pools.YA.updateMember(from, Units.wrap(unitsA));
            pools.YB.updateMember(from, Units.wrap(unitsB));
            pools.YC.updateMember(from, Units.wrap(unitsC));

            // (,,FlowRate dr1) = b.token.distributeFlow(address(this), b.pool, SWAP_DISTRIBUTE_FLOW_ID, dr0 + ar0);
            // _adjustFlowRemainder(b, FlowRate.wrap(0), dr1 - dr0);
        }
    }

    // /**
    //  * @dev Helper function for adjusting flow remainder for the new receiver
    //  *
    //  * @params a - side of the pool to be adjusted,
    //  * @params ird - input rate delta.
    //  * @params drd - flow distribution rate delta before/after flow distribution update.
    //  */
    // function _adjustFlowRemainder(Side storage a, FlowRate ird, FlowRate drd) internal {
    //     // * Given definitions:
    //     //
    //     // 1) ird, drd defined in parameters. Additionally,
    //     // 2) br0: flow rate to the current remainder flow receiver.
    //     // 3) crd: the control flow rate delta. Control flow is flow from aqueduct to the pool itself.
    //     // 4) jrd: pool adjustment flow rate delta.
    //     // 5) ar0: current remainder flow rate to the new remainder receiver.
    //     // 6) ar1: new remainder flow rate to the new remainder receiver.
    //     //
    //     // * We have:
    //     //
    //     // a) when changing remainder flow receiver:
    //     //      ird + jrd = ar1 - ar0 - br0 + crd && ar0 == 0
    //     //    otherwise:
    //     //      ird + jrd = ar1 - br0 + crd
    //     //    so that net flow rate to aqueduct is always 0
    //     // b) crd = drd + jrd
    //     //    so that netflow rate to the pool is always 0
    //     //
    //     // * Then the solution to ar1 is always:
    //     //
    //     // c) ar1 = ird - drd + br0
    //     FlowRate br0 = a.token.getFlowRate(address(this), feeRecipient, ADJUSTMENT_FLOW_ID);
    //     FlowRate ar1 = ird - drd + br0;
    //     a.token.flow(address(this), feeRecipient, ADJUSTMENT_FLOW_ID, ar1);
    // }
}
