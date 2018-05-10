Haskell Servant React Auth Example
==================================

This applications demonstrates

* A decoupled (i.e. hosted on different ports) backend Haskell Servant API server with a React
  single page application front end.
* Using implict OAuth (via google) and JWT for authentication.
* Custom JS API client code generation.

Most examples of this kind of thing handle authentication by serving up a
standard server OAuth flow from the API backend. I wanted to try and avoid this
coupling by keeping the frontend and the backend separate.

The frontend does use a generated API client from the backend, which is
currently done by writing out a JS file when the backend starts. Better
strategies for publishing this API are discussed in the code.

See comments in the code for explanations.

## Development

Development requires a standard environment for both Haskell (`stack`) and Node (`npm`).

First, replace all instances of `CLIENT_ID` and `API_KEY` with relevant
credentials from your Google developer console.

Then in two different consoles:

    cd api
    stack build
    stack exec haskell-servant-spa-auth-example

    cd frontend
    npm start

For development of the backend, I tend to use `stack ghci` and running `main`
the function. This allows for quick reloading with `C-c` and `:r`, for example
using tmux and vim:

    map <leader>r :w\|:call Send_keys_to_Tmux("C-c")<CR>:call Send_to_Tmux(":r\nmain\n")<CR>

## Not included

* This example does not contain any tests. Testing each component individually
  should be straight forward (and most typical of a micro-service environment
  where components are owned by different teams), with acceptance testing
  ideally [done in
  production](https://medium.com/@copyconstruct/testing-in-production-the-safe-way-18ca102d0ef1).
  Might be interesting to experiment with spinning up both the frontend and
  backend for automated integration testing though.
* Logging isn't thread-safe. The currently used `putStrLn` will often
  intersperse concurrent log entries.
