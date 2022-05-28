# Austin 2022 Hackathon

*   Use `nix run .#marlowe-hackathons:exe:marlowe-hackathons` to generate the examples and write them to [solutions/](solutions/).
*   Use `nix develop` to open a development environment.


## Challenges


### 1. English auctions (difficult)

In live terms, English auctions are where bids are announced by the bidders and winners pay what they bid to receive the object. The common operational method of the format is that it is an ascending bid auction in which bids are open for all to see. The winner is the highest bidder and the price is the highest bid.

Solution: [Haskell](app/EnglishAuction.hs), [Marlowe](solutions/marlowe/EnglishAuction.marlowe), [extended JSON](solutions/extended-json/EnglishAuction.json).


### 2. Dutch auctions (difficult)

Dutch auctions are the reverse of English auctions whereby the price begins high and is systematically lowered until a buyer accepts the price.

Solution: [Haskell](app/DutchAuction.hs), [Marlowe](solutions/marlowe/DutchAuction.marlowe), [extended JSON](solutions/extended-json/DutchAuction.json).


### 3. First-price open-bid (easy)

First-price open-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what they bid.

Solution: [Haskell](app/FirstPriceBid.hs), [Marlowe](solutions/marlowe/FirstPriceBid.marlowe), [extended JSON](solutions/extended-json/FirstPriceBid.json).

A very difficult version of this in Marlowe is the first-price *sealed-bid* auction, where participants commit their bids before any of them reveal their bids.
