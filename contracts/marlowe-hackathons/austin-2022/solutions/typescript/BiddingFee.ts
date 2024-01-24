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

A bidding-fee auction contract for Marlowe.

Characteristic of this contract:
*  A seller auctions one unit of an asset.
*  Any number of bidders bid on the contract.
*  Bids may occur in any order.
*  There are a fixed number of bids (rounds of bidding) allowed.
*  A bid is rejected if it isn't higher than all previous bids.
*  A bid is rejected if it isn't immediately followed by a deposit of the Lovelace that was bid.
*  Each bid entails a non-refundable fee paid to the seller.
*  Remaining funds (minus the fees) are returned to unsuccessful bidders.
*  There is deadline for depositing the asset.
*  Each bidding round has a deadline.

*/

(function (): Contract {

  // The party that sells the item at auction.
  const seller : Party = Role("Seller")

  // The quantity of items that is auctioned.
  const assetAmount : Value = Constant(1)

  // The value of the highest bid.
  const highestBid : ValueId = ValueId("Highest Bid")

  // The cost of each bid.
  const biddingFee : Value = ConstantParam("Bidding Fee")

  // Create the Marlowe contract for a first-price open-bid auction.
  function makeContract(
    nRounds    : Number    // The number of rounds of bidding.
  , nBidders   : Number    // The number of bidders.
  , bidBounds  : Bound     // The range for valid bids, in Lovelace.
  , assetToken : Token     // The token representing the asset being bid upon.
  )            : Contract  // The second-price open-bid auction.
  {
    const bids = Array.from(Array(nBidders).keys(), index => ChoiceId("Bid " + (index+1), Role("Bidder " + (index+1))))
    const deadlines = Array.from(Array(nRounds).keys(), index => TimeParam("Bid Deadline " + (index+1)))
    //Deposit the asset, then make the bids, but close if no one bids.
    return makeAssetDeposit(assetToken
      , makeBids(bidBounds, assetToken, deadlines, bids, Close)
    )
  }

  // Deposit the asset that is the subject of the bidding.
  function makeAssetDeposit(
    asset        : Token     // The token representing the asset being bid upon.
  , continuation : Contract  // The contract to be executed after the asset is deposited.
  )              : Contract  // The contract for the asset deposit and subsequent activity.
  {
    const assetDeadline : Timeout = TimeParam("Deadline to Deposit Asset")
    return When(
      [
        // The seller deposits the asset being auctioned.
        Case(Deposit(seller, seller, asset, assetAmount)
        , continuation
        )
      ]
      // End the contract if the deposit was not made.
    , assetDeadline
    , Close
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
    if (deadlines.length == 0 || bids.length == 0) {
      return continuation
    } else {
      const deadline = deadlines[0]
      const remainingDeadlines = deadlines.slice(1)
      return When(
        bids.map(bid => {
          const bidder = bid.choice_owner
          function remaining(continuation : Contract) : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids, continuation)}
          function disqualify() : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids.filter(bid1 => bid1 != bid), continuation)}
          const bidAmount = ChoiceValue(bid)
          // Let the bidder make their bid.
          return Case(Choice(bid, [bounds])
            // Check if the bid is highest so far.
          , If(ValueGT(bidAmount, UseValue(highestBid))
              // Require a deposit if the bid is highest.
            , When(
                [
                  // Deposit the Lovelace for the bid.
                  Case(Deposit(bidder, bidder, ada, AddValue(SubValue(bidAmount, AvailableMoney(ada, bidder)), biddingFee))
                    // Pay the bidding fee to the seller.
                  , Pay(bidder, Account(seller), ada, biddingFee
                    // Record the new highest amount.
                    , Let(highestBid, bidAmount
                      // Handle the remaining bids.
                      , remaining(
                          // Make the payment for the asset.
                          Pay(bidder, Account(seller), ada, bidAmount
                          , Pay(seller, Party(bidder), assetToken, assetAmount
                            , Close
                            )
                          )
                        )
                      )
                    )
                  )
                ]
                // Ignore the bid and disqualify the bidder if the deposit was not made.
              , deadline
              , disqualify()
              )
              // Ignore the bid and disqualify the bidder if it is not highest.
            , (
              // Handle the remaining bids and finalization.
              disqualify()
              )
            )
          )
        })
        // End the bidding if no one bids in this round.
      , deadline
      , continuation
      )
    }
  }

  // Create an example contract.
  const example : Contract = makeContract(3, 3, Bound(2000000, 1000000000000), Token("1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada".toLowerCase(), "The Asset"))

  return example

})
