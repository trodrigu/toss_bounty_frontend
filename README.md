# TossBounty Elm App

# Getting started

If you don't already have `elm` and `elm-live`:

> npm install -g elm elm-live

# Dependencies

  * create a Stripe [account](https://dashboard.stripe.com/register) and grab the client id
  * add a .env file at the root with the following

  ```
  export API_URL=http://localhost:4000
  export STRIPE_PLATFORM_CLIENT_ID=
  ```

# Building and running

  * `source .env` to read env vars into the environment

> make