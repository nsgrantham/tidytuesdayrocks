#!/usr/bin/env bash

# This script authorizes your Twitter Developer app with OAUTH2
# https://developer.twitter.com/en/docs/basics/authentication/api-reference/token.html

# Make sure you define the following two environment variables:
# export TWITTER_CONSUMER_API_KEY=<YOUR 25-CHARACTER KEY HERE>
# export TWITTER_CONSUMER_API_SECRET_KEY=<YOUR 50-CHARACTER KEY HERE>
# Both can be found under the "Keys and tokens" tab when viewing app details.

bearer_token="$TWITTER_CONSUMER_API_KEY:$TWITTER_CONSUMER_API_SECRET_KEY"

base64_encoded_bearer_token=$(echo -n $bearer_token | openssl base64)

authorization="Authorization: Basic $base64_encoded_bearer_token"
echo $authorization

curl -X POST https://api.twitter.com/oauth2/token \
    -H $authorization \
    -H 'Content-Type: application/x-www-form-urlencoded;charset=UTF-8' \
    -d 'grant_type=client_credentials'


