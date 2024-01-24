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

A second-price open-bid auction contract for Marlowe.

Characteristic of this contract:
*  A seller auctions one unit of an asset.
*  Any number of bidders bid on the contract.
*  Bids may occur in any order.
*  A bid is rejected if it isn't the highest or second highest of the bids so far.
*  A bid is rejected if it isn't immediately followed by a deposit of the Lovelace that was bid.
*  Funds are returned to unsuccessful bidders.
*  There is deadline for depositing the asset.
*  Each bidding round has a deadline.
*  Bidders may only bid once.
*  The winner pays the value of the second highest of the bids, and receives a refund of the difference between the highest and second highest.
*  If there is only one bid, the winner pays their bid.

*/

(function (): Contract {

  // The party that sells the item at auction.
  const seller : Party = Role("Seller")

  // The quantity of items that is auctioned.
  const assetAmount : Value = Constant(1)

  // The value of the highest bid.
  const highestBid : ValueId = ValueId("Highest Bid")

  // The value of the second highest bid.
  const secondHighestBid : ValueId = ValueId("Second highest Bid")

  // Create the Marlowe contract for a first-price open-bid auction.
  function makeContract(
    n          : Number    // The number of bidders.
  , bidBounds  : Bound     // The range for valid bids, in Lovelace.
  , assetToken : Token     // The token representing the asset being bid upon.
  )            : Contract  // The second-price open-bid auction.
  {
    const bids = Array.from(Array(n).keys(), index => ChoiceId("Bid " + (index+1), Role("Bidder " + (index+1))))
    const deadlines = Array.from(Array(n).keys(), index => TimeParam("Bid Deadline " + (index+1)))
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
        permutations(bids).map(bid1 => {
          const bid = bid1[0]
          const remainingBids = bid1.slice(1)
          const bidder = bid.choice_owner
          function remaining(continuation : Contract) : Contract {return makeBids(bounds, assetToken, remainingDeadlines, remainingBids, continuation)}
          function disqualify() : Contract {return makeBids(bounds, assetToken, remainingDeadlines, bids.filter(bid1 => bid1 != bid), continuation)}
          const bidAmount = ChoiceValue(bid)
          const winAmount = Cond(ValueEQ(UseValue(secondHighestBid), Constant(0))
          , UseValue(highestBid)
          , UseValue(secondHighestBid)
          )
          // Let the bidder make their bid.
          return Case(Choice(bid, [bounds])
            // Check if the bid is highest or second highest so far.
          , If (ValueGT(bidAmount, UseValue(secondHighestBid))
              // Require a deposit if the bid is highest or second highest.
            , When(
                [
                  // Deposit the Lovelace for the bid.
                  Case(Deposit(bidder, bidder, ada, bidAmount)
                    // Check if the bid is highest so far.
                  , If(ValueGT(bidAmount, UseValue(highestBid))
                      // Record the new second highest amount.
                    , Let(secondHighestBid, UseValue(highestBid)
                        // Record the new highest amount.
                      , Let(highestBid, bidAmount
                          // Handle the remaining bids.
                        , remaining(
                            // Make the payment for the asset.
                            Pay(bidder, Party(seller), ada, winAmount
                            , Pay(seller, Party(bidder), assetToken, assetAmount
                              , Close
                              )
                            )
                          )
                        )
                      )
                    , (
                        // Record the new second highest amount.
                        Let(secondHighestBid, bidAmount
                          // Handle the remaining bids.
                        , remaining(continuation)
                        )
                      )
                    )
                  )
                ]
                // Ignore the bid and disqualify the bidder if the deposit was not made.
              ,  deadline
              , disqualify()
              )
            , (
                // Handle the remaining bids.
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

  // Compute permutations. Source: <https://stackoverflow.com/questions/40654895/javascript-generating-all-permutations-of-an-array>.
  function permutations(array : ChoiceId[]) : ChoiceId[][]
  {
    let result : ChoiceId[][] = [];
    function p(array : ChoiceId[], temp : ChoiceId[]) {
        if (!array.length) {
            result.push(temp);
        }
        for (let i = 0; i < array.length; i++) {
            let x = array.splice(i, 1)[0];
            p(array, temp.concat(x));
            array.splice(i, 0, x);
        }
    }
    p(array, []);
    return result;
  }

  // Create an example contract.
  const example : Contract = makeContract(3, Bound(2000000, 1000000000000), Token("1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada".toLowerCase(), "The Asset"))

  return example

})
