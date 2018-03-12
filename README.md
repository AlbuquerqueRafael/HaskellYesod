## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Install PostgreSql and configure according with the following link: https://github.com/yesodweb/yesod/wiki/Setting-up-PostgreSQL
4. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically be recompiled and redeployed to localhost.

## Tests

```
stack test --flag functional-project:library-only --flag functional-project:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.

## Turn on the Google Sheets API

* Use [this wizard](https://console.developers.google.com/start/api?id=sheets.googleapis.com&hl=pt-br) to create or select a project in the Google Developers Console and automatically turn on the API. Click `Continue`, then `Go to credentials`.
* On the `Add credentials to your project` page, click the `Cancel` button.
* At the top of the page, select the `OAuth consent screen` tab. Select an `Email address`, enter a `Product name` if not already set, and click the `Save` button.
* Select the `Credentials` tab, click the `Create credentials` button and select `OAuth client ID`.
* Select the application type `Other`, enter the name "NAME OF YOUR APPLICATION", and click the `Create` button.
* Click the `(Download JSON)` button to the right of the client ID.
* Move this file to the directory `~/.config/gcloud/` and rename it `application_default_credentials.json`.
* You must also share with your service the spreadsheet that you want to get the info of.
* In order to do this you must share the sheet with the email address of your service which is in your downloaded service config file.
