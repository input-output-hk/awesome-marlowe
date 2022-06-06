import {
    PK, Role, Account, Party, ada, AvailableMoney, Constant, ConstantParam,
    NegValue, AddValue, SubValue, MulValue, DivValue, ChoiceValue, TimeIntervalStart,
    TimeIntervalEnd, UseValue, Cond, AndObs, OrObs, NotObs, ChoseSomething,
    ValueGE, ValueGT, ValueLT, ValueLE, ValueEQ, TrueObs, FalseObs, Deposit,
    Choice, Notify, Close, Pay, If, When, Let, Assert, SomeNumber, AccountId,
    ChoiceId, Token, ValueId, Value, EValue, Observation, Bound, Action, Payee,
    Case, Timeout, ETimeout, TimeParam, Contract
} from 'marlowe-js';

(function (): Contract {

  // The party that sells the item at auction.
  const seller : Party = Role("Seller")

  // The quantity of items that is auctioned.
  const assetAmount : Value = Constant(1n)

  // The value of the highest bid.
  const highestBid : ValueId = "Highest Bid"

  // Create the Marlowe contract for an English auction.
  function makeContract(
    nRounds    : Number    // The number of rounds of bidding.
  , nBidders   : Number    // The number of bidders.
  , bidBounds  : Bound     // The range for valid bids, in Lovelace.
  , assetToken : Token     // The token representing the asset being bid upon.
  )            : Contract  // The English auction.
  {
    const bids = Array.from(Array(nBidders).keys(), index => ChoiceId("Bid " + (index+1), Role("Bidder " + (index+1))))
    const deadlines = Array.from(Array(nRounds).keys(), index => TimeParam("Bid Deadline " + (index+1)))
    // Deposit the asset, then make the bids, but close if no one bids.
    return makeAssetDeposit(
      assetToken
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
        Case (Deposit(seller, seller, asset, assetAmount), continuation)
      ]
      // The contract ends if the deposit is not made.
    ,  assetDeadline
    ,  Close
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
        bids.map(
          bid => {
            const bidder = bid.choice_owner
            const bidAmount = ChoiceValue(bid)
            function remaining(continuation : Contract) : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids, continuation)}
            // Let the bidder make their bid.
            return Case(Choice(bid, [bounds])
            // Check if the bid is highest so far.
            , If(ValueGT(bidAmount, UseValue(highestBid))
              // Require a deposit if the bid is highest.
              , When(
                  [
                    // Deposit the Lovelace for the bid.
                    Case(Deposit(bidder, bidder, ada, SubValue(bidAmount, AvailableMoney(ada,bidder)))
                      // Record the new highest amount.
                    , Let(highestBid, bidAmount
                      // Handle the remaining bids.
                      , remaining(
                        // Make the payment for the asset.
                        Pay(bidder, Party(seller), ada, bidAmount
                        , Pay(seller, Party(bidder), assetToken, assetAmount
                          , Close
                          )
                        ))
                      )
                    )
                  ]
                // Ignore the bid if the deposit was not made.
                , deadline
                , remaining(continuation)
                )
              // Ignore the bid if it is not highest.
              // Handle the remaining bids and finalization.
              ,  remaining(continuation)
              )
            )
          }
        )
      , deadline
      , continuation
      )
    }
  }

  const example = makeContract(3, 3, Bound(2000000, 1000000000000), Token("1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada".toLowerCase(), "The Asset"))

  return example

})
