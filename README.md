# Concordium Dashboard

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

Network and Chain Dashboard for displaying stats, summaries, and visualizations of the blockchain network and state.

- Network: A table view of nodes which are participating in the data collection.
  Clicking a node in the table opens a full screen view with the details of that node.
- Chain: A live visualization of the chain which displays new blocks as they're being submitted to the network.
  The blocks are colored based on their finalization status and a number of dots illustrate the
  consensus that has been reached around a given block.
  Clicking a block displays the contents of the block in the Node Explorer below the visualization.

The app consists of a server written in [TypeScript](https://github.com/Microsoft/TypeScript)
and a client written in [Elm](https://elm-lang.org/) (and packaged using [webpack](https://webpack.github.io/)).
The client is compiled into a few HTML and JavaScript files.
The server is very basic and just registers a router for serving these static files.


### Install and run

From the project root, run
```
npm install
```

To build and run the app in watch mode with source maps, run
```
npm run dev
```
Then open the app (http://localhost:3001) in a browser. The app refreshes automatically when files are changed.


Other common build/run targets include:

- `npm test` - Run jest tests (currently fails because there are no tests!).
- `npm run build` - Build the app in production mode and put it into `./dist`.
- `npm start` - Start the app (shorthand for `node ./dist/server/server.js`).

See the `script` section of `package.json` for all targets as well as their definitions.


### Configuration

The client app may be built in production or development mode (based on the `NODE_ENV` environment variable).
In production mode (which staging also uses), the collector/middleware backends are assumed to reside on the same 
domain as the dashboard itself. In development mode, the target backend (local, staging, or production) may be set
by changing the value of `devTarget` in `src/client/elm/Config.elm`.


### Architecture

The dashboard collects data from two different backend services:

- Collector Backend: All current state (participating nodes and their stats, including their best block etc.).
  This data is polled (every second).
- Middleware: Block contents for the Block Explorer.

Note that the dashboard server only serves the frontend application itself - all dynamic data come from the services above.

A complete setup consists of ([diagram](https://docs.google.com/drawings/d/1FWV8Ah9RAiqMaghT3Ql1JyGnBq0_TxOS6BgM6mFjepQ/edit)):

- One or more nodes.
- One collector for each node that we want to participate data. This collector polls the node and pushes the state to the collector backend.
- One collector backend for keeping the state from the collectors.
- One middleware instance connected to one of the nodes.


#### Local setup

Detailed instructions on setting up a local node/baker are given in the readme of
the [concordium-node](https://github.com/Concordium/concordium-node/tree/main/concordium-node) repository.

The easiest solution is to start a local cluster using the docker-compose scripts in the root of that repo.
This will start all components except for the middleware. The scripts support multiple different cluster sizes;
to spin up 5 nodes, use:

```
EXTRA_ARGS=--debug NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.yml up --scale baker=5 --force-recreate
```

The middleware lives in the [concordium-client](https://github.com/Concordium/concordium-client) repo
and may be started (from the root of that project) using the command
```
NODE_URL=127.0.0.1:$PORT stack run middleware
```
where `$PORT` is the GRPC port of the node (default: 10000).

If the docker-compose scripts are used, then `scripts/bootMiddleware.sh` will run the above command with a working port.

The node and [collector](https://github.com/Concordium/concordium-node/blob/main/concordium-node/src/bin/collector.rs)
([backend](https://github.com/Concordium/concordium-node/blob/main/concordium-node/src/bin/collector_backend.rs)) is defined in the
[concordium-node](https://github.com/Concordium/concordium-node/tree/main/concordium-node) repo.

Note that you need to set a feature flag to build the collector (backend).


### Docker build

The Dashboard is deployed on Kubernetes using a dockerized build which is build by `./docker.sh`.
Builds `dist` locally first before copying into image. 


#### Requirements and credits

- Node 12+

- The project structure was initially derived from [fullstack-typescript](https://github.com/gilamran/fullstack-typescript),
replacing React with Elm.
