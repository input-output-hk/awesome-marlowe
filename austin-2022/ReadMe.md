# Austin 2022 Hackathon

*   Use `nix run .#marlowe-hackathons:exe:marlowe-hackathons` to generate the examples and write them to [solutions/](solutions/).
*   Use `nix develop` to open a development environment.


## Challenges


### 1. English auctions (difficult)

In live terms, English auctions are where bids are announced by the bidders and winners pay what they bid to receive the object. The common operational method of the format is that it is an ascending bid auction in which bids are open for all to see. The winner is the highest bidder and the price is the highest bid.

Solution: [Haskell](app/EnglishAuction.hs), [Marlowe](solutions/marlowe/EnglishAuction.marlowe), [core JSON](solutions/core-json/EnglishAuction.json) [extended JSON](solutions/extended-json/EnglishAuction.json), [TypeScript](solutions/typescript/EnglishAuction.ts).


### 2. Dutch auctions (difficult)

Dutch auctions are the reverse of English auctions whereby the price begins high and is systematically lowered until a buyer accepts the price.

Solution: [Haskell](app/DutchAuction.hs), [Marlowe](solutions/marlowe/DutchAuction.marlowe), [core JSON](solutions/core-json/DutchAuction.json), [extended JSON](solutions/extended-json/DutchAuction.json), [TypeScript](solutions/typescript/DutchAuction.ts).


### 3. First-price open-bid (easy)

First-price open-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what they bid. This contract is not realistic because it can easily be gamed by waiting to bid last.

Solution: [Haskell](app/FirstPriceBid.hs), [Marlowe](solutions/marlowe/FirstPriceBid.marlowe), [core JSON](solutions/core-json/FirstPriceBid.json), [extended JSON](solutions/extended-json/FirstPriceBid.json), [TypeScript](solutions/typescript/FirstPrice.ts).

A very difficult variant of this in Marlowe is the first-price *sealed-bid* auction, where participants commit their bids before any of them reveal their bids.


### 4. Second-price open-bid (intermediate)

Second-price open-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what the second-highest bidder bid. This contract is not realistic because it can easily be gamed through collusion.

Solution: [Haskell](app/SecondPriceBid.hs), [Marlowe](solutions/marlowe/SecondPriceBid.marlowe), [core JSON](solutions/core-json/SecondPriceBid.json), [extended JSON](solutions/extended-json/SecondPriceBid.json), [TypeScript](solutions/typescript/SecondPrice.ts).

A very difficult variant of this in Marlowe is the Vickrey second-price *sealed-bid* auction, where participants commit their bids before any of them reveal their bids.


### 5. Reverse auction (intermediate)

Reverse auctions are where the roles of buyer and seller are reversed. Multiple sellers compete to obtain the buyer's business and prices typically decrease over time as new offers are made.

Solution: [Haskell](app/ReverseAuction.hs), [Marlowe](solutions/marlowe/ReverseAuction.marlowe), [core JSON](solutions/core-json/ReverseAuction.json), [extended JSON](solutions/extended-json/ReverseAuction.json), [TypeScript](solutions/typescript/ReverseAuction.ts).


### 6. Bidding fee auction (difficult)

A bidding fee auction (also known as a penny auction) requires customers to pay the auction owner for bids, which they can increment an auction price one unit of currency at a time.  On English auctions for example, the price goes up in 1 pence (0.01 GBP) increments.

Solution: [Haskell](app/BiddingFee.hs), [Marlowe](solutions/marlowe/BiddingFee.marlowe), [core JSON](solutions/core-json/BiddingFee.json), [extended JSON](solutions/extended-json/BiddingFee.json).


### 7. Stable coin (easy)

The Marlowe analog of a stable coin is a contract that pays the party the value in a base currency (say, USD) of the ADA that they initially deposit in a contract. A counterparty deposits collateral, according to a specified *reserve ratio* (i.e., the ratio of the total ADA in the contract to the ADA deposited by the party), to cover price changes in ADA. When the party withdraws their funds, they receive the ADA corresponding to the base-currency value initially deposited, and the counterparty receives the remaining ADA.

Solution: [Haskell](app/StableCoin.hs), [Marlowe](solutions/marlowe/StableCoin.marlowe), [core JSON](solutions/core-json/StableCoin.json), [extended JSON](solutions/extended-json/StableCoin.json), [TypeScript](solutions/typescript/StableCoin.ts).
