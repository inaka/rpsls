[Rock Paper Scissors Lizzard Spock](http://en.wikipedia.org/wiki/Rock-paper-scissors-lizard-Spock) World Championship
====

The Original World Championship took place on Tuesday March 27th, 2012.

The prize was a ticket for that year's Erlang Factory in San Francisco.

The winner was [George Ye](https://github.com/georgeye), with [`rpsls_beatself_player`](src/players/rpsls_beatself_player.erl).

The original rules were:

>To play this game just create a module that implements the [*rpsls_player*](https://github.com/inaka/rpsls/blob/master/src/rpsls_player.erl) behaviour, submit a pull-request, and we'll add it to the competition.

>* Sponsored by [Inaka](http://inakanetworks.com)
>* You can see some sample implementations [here](https://github.com/inaka/rpsls/tree/master/src/players)
>* If only one person enters, that person will win the ticket.
>* The winner has the option to take our Erlang Factory 2012 ticket.
>* If the winner cannot *personally* go, they must inform us so we can give it to someone else.
>* Entries must compile to BEAM code but can be in any language that can do so - efenne, LFE, Elixir, etc.
>* Thanks to SpawnFest, which granted us the ticket, but we don't need it (we're already going) so we're having fun giving it away!
>* Our decision is final!

## You can still play!

You can play on your computer against any existing player. To do that, start the shell running `rebar3 shell`, then start the app with `rpsls:start().` and use `rpsls:play/3` to play (e.g. `rpsls:play(rpsls_bart_player, rpsls_lisa_player, 10).`).

### Submit your Player

If you created a player module you're proud of, please submit it to the repo with a PR. Others might be interested in playing against it. :)

---

## Contact Us
If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/rpsls/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)
