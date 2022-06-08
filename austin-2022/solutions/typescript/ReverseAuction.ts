import {
    PK, Role, Account, Party, ada, AvailableMoney, Constant, ConstantParam,
    NegValue, AddValue, SubValue, MulValue, DivValue, ChoiceValue, TimeIntervalStart,
    TimeIntervalEnd, UseValue, Cond, AndObs, OrObs, NotObs, ChoseSomething,
    ValueGE, ValueGT, ValueLT, ValueLE, ValueEQ, TrueObs, FalseObs, Deposit,
    Choice, Notify, Close, Pay, If, When, Let, Assert, SomeNumber, AccountId,
    ChoiceId, Token, ValueId, Value, EValue, Observation, Bound, Action, Payee,
    Case, Timeout, ETimeout, TimeParam, Contract
} from 'marlowe-js';

/*

A reverse auction contract for Marlowe.

 Characteristic of this contract:
 *  Sellers auctions one unit of an asset.
 *  Any number of sellers offer on the contract.
 *  Bids may occur in any order.
 *  There are a fixed number of bids (rounds of bidding) allowed.
 *  A bid is rejected if it isn't lower than all previous bids.
 *  A bid is rejected if it isn't immediately followed by a deposit of the asset, if the bidder didn't already deposit the asset.
 *  Assets are returned to unsuccessful bidders.
 *  Each bidding round has a deadline.

*/

(function (): Contract {

// The party that buys the item at auction.
const buyer : Party = Role("Buyer")

// The quantity of items that is auctioned.
const assetAmount : Value = Constant(1)

// The value of the highest bid.
const lowestOffer : ValueId = ValueId("Lowest Offer")

// Create the Marlowe contract for a reverse auction.
  function makeContract(
    nRounds    : Number    // The number of rounds of bidding.
  , nBidders   : Number    // The number of bidders.
  , bidBounds  : Bound     // The range for valid bids, in Lovelace.
  , assetToken : Token     // The token representing the asset being bid upon.
  )            : Contract  // The reverse auction.
  {
    const bids = Array.from(Array(nBidders).keys(), index => ChoiceId("Bid " + (index+1), Role("Bidder " + (index+1))))
    const deadlines = Array.from(Array(nRounds).keys(), index => TimeParam("Bid Deadline " + (index+1)))
    // Make the bids, but close if no one bids.
    return makeBids(bidBounds, assetToken, deadlines, bids, Close)
  }

  // Deposit the asset that is the subject of the bidding.
  function ensureAssetDeposit(
    asset                  : Token     // The token representing the asset being bid upon.
  , deadline               : Timeout   // The deadline for the deposit.
  , bidder                 : Party     // The bidder.
  , depositContinuation    : Contract  // The contract to be executed after the asset is deposited.
  , notDepositContinuation : Contract  // The contract to be executed if the asset is not deposited.
  )                        : Contract  // The contract for the asset deposit and subsequent activity.
  {
    return If(ValueEQ(AvailableMoney(asset, bidder), assetAmount)
    // No deposit is necessary.
    ,  depositContinuation
    // The bidder must deposit the asset.
    , When(
        [
          Case(Deposit(bidder, bidder, asset, assetAmount)
          , depositContinuation
          )
        ]
      , deadline
      , notDepositContinuation
      )
    )
  }

  // Make the contract for bids.
  function makeBids(
    bounds       : Bound       // The range of valid bids, in Lovelace.
  , assetToken   : Token       // The token representing the asset being bid upon.
  , deadlines    : Timeout[]   // The deadlines for the rounds of bidding.
  , bids         : ChoiceId[]  // The choices the bidders will make.
  , continuation : Contract    // The contract to be executed at the end of the bidding.
  )              : Contract    // The bidding contract.
  {
    if (deadlines.length == 0 || bids.length == 0){
      return continuation
    } else {
      const deadline = deadlines[0]
      const remainingDeadlines = deadlines.slice(1)
      return When(
        bids.map(bid => {
          const bidder = bid.choice_owner
          const bidAmount = ChoiceValue(bid)
          function remaining(continuation : Contract) : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids, continuation)}
          function disqualify() : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids.filter(bid1 => bid1 != bid), continuation)}
          // Let the bidder make their bid.
          return Case(Choice(bid, [bounds])
            // Check if the bid is lowest so far.
          , If(OrObs(ValueLT(bidAmount, UseValue(lowestOffer)), ValueEQ(Constant(0), UseValue(lowestOffer)))
                // Require a deposit if the bid is lowest.
              , ensureAssetDeposit(assetToken, deadline, bidder
                  // Record the new highest amount.
                , Let(lowestOffer, bidAmount
                    // Let the buyer purchase the asset.
                  , When(
                      [
                        Case(Deposit(bidder, buyer, ada, bidAmount)
                          // Deliver the asset to the buyer.
                          , Pay(bidder, Party(buyer), assetToken, assetAmount
                            , Close
                            )
                        )
                      ]
                      // Handle the remaining bids.
                    , deadline
                    , remaining(continuation)
                    )
                  )
                // Ignore the bid and disqualify the bidder if it is not lowest.
                , (
                    // Handle the remaining bids.
                    disqualify()
                  )
                )
              // Ignore the bid and disqualify the bidder if it is not lowest.
            , (
                // Handle the remaining bids.
                disqualify()
              )
            )
          )
        })
      , deadline
      , continuation
      )
    }
  }

  // Create an example contract.
  const example : Contract = makeContract(3, 3, Bound(2000000, 1000000000000), Token("1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada".toLowerCase(), "The Asset"))

  return example

})
