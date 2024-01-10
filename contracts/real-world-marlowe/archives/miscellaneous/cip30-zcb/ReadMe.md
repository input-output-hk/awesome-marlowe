# Example of Using Marlowe Runtime with a CIP30 Wallet

This example shows how to use a Babbage-compatible [CIP30](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) wallet such as [Nami](https://namiwallet.io/) to sign Marlowe transactions. The example contract here simply receives a deposit and waits until a specified time before the funds become payable to an address.

[This video](https://youtu.be/8Nf7nYuAK6I) shows the application in action.


## CIP30 API

Only three parts of the CIP30 API are used by this example:

- The application connects with `cardano.{walletName}.enable({ extensions: Extension[] } = {}): Promise<API>`.
- A call to `api.getChangeAddress(): Promise<Address>` provides the change address used by Marlowe Runtime.
- Calls to `api.signTx(tx: cbor<transaction>, partialSign: bool = false): Promise<cbor<transaction_witness_set>>` return a witness to the transaction, which Marlowe Runtime has prepared and later submits.


## Marlowe Runtime API

Marlowe Runtime's POST endpoints for creating transactions will return the unsigned transaction as a result (instead of transaction body) if a particular header is present:

- Contracts endpoint uses "Accept: application/vendor.iog.marlowe-runtime.contract-tx-json" header.
- Transactions endpoint uses "Accept: application/vendor.iog.marlowe-runtime.apply-inputs-tx-json" header.
- Withdraw endpoint uses "Accept: application/vendor.iog.marlowe-runtime.withdraw-tx-json" header.

All the above endpoints also accept witnessset as a payload for PUT request.


## Source files

- [index.html](index.html): The HTML page for the application.
- [view.css](view.css): The CSS styling for the application
- [src/controller.js](src/controller.js): The JavaScript source code for the application.


## Running the application

- If you have Nix installed, simply execute [./run.sh](run.sh).
- Alternatively, if you have NodeJS installed, execute `npm install` and then `npx webpack-dev-server`.

The application is served from [http://127.0.0.1:3000](http://127.0.0.1:3000). It requires the following:

- [Eternl](https://eternl.io/) wallet is installed in the web browser. (This web application was tested against Eternl 1.11.13.)
- Eternl is connected to an address with at least 150 ada on the Cardano preproduction network.
