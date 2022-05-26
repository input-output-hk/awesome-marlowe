# Austin 2022 Hackathon

*   Use `nix develop` to open a development environment.
*   Use `nix run .#marlowe-hackathons:exe:marlowe-hackathons` to generate the examples and write them to [solutions/](solutions/).


## Challenges


### 1. English auctions (difficult)

In live terms, English auctions are where bids are announced by the bidders and winners pay what they bid to receive the object. 
The common operational method of the format is that it is an ascending bid auction in which bids are open for all to see. 
The winner is the highest bidder and the price is the highest bid.

Solution: [Haskell](app/EnglishAuction.hs) [JSON](solutions/EnglishAuction.json)
