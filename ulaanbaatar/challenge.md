# Frontier Fintech Marlowe Hackathon Challenge

The following paragraphs describe the six most common forms of online auctions.
Each team should pick one of the six and try to implement it as a Marlowe
contract.

To simplify matters, we can assume that there are always exactly three bidders.

You may need to add deadlines or make other small modifications to make
conversion to Marlowe feasible! You can also make simplifying assumptions if
necessary.

## 1. English auctions (difficult)
In live terms, English auctions are where bids are announced by the bidders and winners pay what they bid to receive the object. 
The common operational method of the format is that it is an ascending bid auction in which bids are open for all to see. 
The winner is the highest bidder and the price is the highest bid.

## 2. Dutch auctions (intermediate)
Dutch auctions are the reverse of English auctions whereby the price begins high and is systematically lowered until a buyer accepts the price. 

## 3. First-price sealed-bid (easy)
First-price sealed-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what they bid. 

## 4. Vickrey auction (intermediate)
A Vickrey auction, sometimes known as a second-price sealed-bid auction, uses very much the same principle as a first-price sealed bid. 
However, the highest bidder and winner will only pay what the second highest bidder had bid. 

## 5. Reverse auction (intermediate)
Reverse auctions are where the roles of buyer and seller are reversed. Multiple sellers compete to obtain the buyer's business and prices typically decrease over time as 
new offers are made.

## 6. Bidding fee auction (difficult)
A bidding fee auction (also known as a penny auction) requires customers to pay the auction owner for bids, 
which they can increment an auction price one unit of currency at a time.  On English auctions for example, the price goes up in 1 pence (0.01 GBP) increments.
