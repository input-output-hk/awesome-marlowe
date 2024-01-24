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

A Dutch auction contract for Marlowe.

Characteristic of this contract:
*  A seller auctions one unit of an asset.
*  Any number of bidders bid on the contract.
*  The selling price is dropped in equal steps.
*  The first bidder to deposit the price wins the asset.
*  There is deadline for depositing the asset.
*  Each bidding round has a deadline.

*/

(function (): Contract {

  // The party that sells the item at auction.
  const seller : Party = Role("Seller")

  // The quantity of items that is auctioned.
  const assetAmount : Value = Constant(1n)

  // Create the Marlowe contract for a Dutch auction.
  function makeContract(
    n          : Number    // The number of bidders.
  , bound      : Bound     // The range for valid bids, in Lovelace.
  , assetToken : Token     // The token representing the asset being bid upon.
  , steps      : Number    // The number of steps of lowering the price from the top to the bottom of the range.
  )            : Contract  // The Dutch auction.
  {
    const minimumPrice = BigInt(bound.from)
    const maximumPrice = BigInt(bound.to)
    const bidders = Array.from(Array(n).keys(), index => Role("Bidder " + (index+1)))
    const delta : bigint = (maximumPrice - minimumPrice) / (BigInt(steps) - 1n)
    const prices = Array.from(Array(steps).keys(), index => Constant(minimumPrice + (BigInt(steps) - BigInt(index) - 1n) * delta))
    console.log("P " + prices)
    prices[0] = Constant(maximumPrice)
    const deadlines = Array.from(Array(steps).keys(), index => TimeParam("Bid Deadline " + (index+1)))
    // Deposit the asset, then make the bids, but close if no one bids.
    return makeAssetDeposit(assetToken, makeBids(assetToken, deadlines, prices, bidders, Close))
  }


  // Deposit the asset that is the subject of the bidding.
  function makeAssetDeposit(
    asset        : Token     // The token representing the asset being bid upon.
  , continuation : Contract  // The contract to be executed after the asset is deposited.
  ) : Contract               // The contract for the asset deposit and subsequent activity.
  {
    const assetDeadline : Timeout = TimeParam("Deadline to Deposit Asset")
    return When(
      [
        // The seller deposits the asset being auctioned.
        Case(Deposit(seller, seller, asset, assetAmount)
        , continuation
        )
      ]
      // Otherwise, the contract ends.
    , assetDeadline
    , Close
    )
  }

  // Make the contract for bids.
  function makeBids(
    assetToken   : Token      // The token representing the asset being bid upon.
  , deadlines    : Timeout[]  // The deadlines for the rounds of bidding.
  , prices       : Value[]    // The prices for the deadlines.
  , bidders      : Party[]    // The bidders.
  , continuation : Contract   // The contract to be executed at the end of the bidding.
  )                : Contract   // The bidding contract.
  {
    if (deadlines.length == 0 || prices.length == 0 || bidders.length == 0) {
      return continuation
    } else {
      const deadline = deadlines[0]
      const remainingDeadlines = deadlines.splice(1)
      const price = prices[0]
      const remainingPrices = prices.splice(1)
      return When(
        bidders.map(bidder =>
          // The winning bidder makes their deposit.
          Case(Deposit(bidder, bidder, ada, price)
          , Pay(bidder, Party(seller), ada, price
            , Pay(seller, Party(bidder), assetToken, assetAmount
              , Close
              )
            )
          )
        )
      // Continue the bidding if no one bids in this round.
      , deadline
      , makeBids(assetToken, remainingDeadlines, remainingPrices, bidders, continuation)
      )
    }
  }

  // Create an example contract.
  const example : Contract = makeContract(3, Bound(2000000, 1002000000), Token("1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada".toLowerCase(), "The Asset"), 5)

  return example

})
