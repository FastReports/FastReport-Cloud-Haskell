# Auto-Generated OpenAPI Bindings to `FastReportCloud`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the FastReportCloud API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/FastReports/FastReport-Cloud-Haskell
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `FastReportCloud.API` module, which exports the FastReportCloudBackend type. The FastReportCloudBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createFastReportCloudClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runFastReportCloudClientWithManager` or more conveniently with `callFastReportCloudClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `https://fastreport.cloud/` is serving the FastReportCloud API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import FastReportCloud.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "https://fastreport.cloud/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  FastReportCloudBackend{..} <- API.createFastReportCloudClient

  -- Any FastReportCloud API call can go here, e.g. here we call `getSomeEndpoint`
  API.callFastReportCloud (mkClientEnv manager url) getSomeEndpoint
```

## Authors

Fast Reports team https://www.fast-report.com/en/
