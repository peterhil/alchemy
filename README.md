# Alchemy

A game of [alchemy] of the soul, in the sense of [Magnum Opus] (The Great Work) and hermetic tradition.

## Symbology and philosophy

The sephiroth have correspondances to [planets] among other things. The
birds have [symbolical meanings] in alchemy.

## Game play

### Objective

You start as a black crow jumping around the hexagonal map, first collecting gems, and then other things that are related to [the ten holy sephiroth].

You advance on the [Tree of Life] and try to reach the [Kether] (crown) state through Understanding (Binah) and wisdom (Chochmah) and other [sephiroth], starting from Malkuth (Kingdom).

Take care to keep the tree **in balance** – otherwise you might **slip back levels** and revert to previous stages on the tree and as a symbolical bird.

When you reach [Kether], you win.

### Keeping balance

To keep balance, you need to have a solid base.

So always collect at least the same amount of things related to lower sephiroth as those related to higher levels. This also keeps the sephiroth horizontally balanced on the Tree of Life.

Chaos ensues, if you accidentally collect more things on the higher levels – and you slip down back to a lower level, that still has balance and support of the Foundation. You lose the things collected above that level.

### Movement

The basic movements are **UP** and **DOWN** arrow keys, and the **A**, **S**, **X**, **Z** keys for diagonal movements.

**Movement gets easier** as you progress: On Yesod (Foundation) level, you can **wrap around** the board edges, and on Tiphereth level you can move sideways with **LEFT** and **RIGHT** arrow keys.


## Links

Find [Alchemy on Itch](https://peterhil.itch.io/alchemy) or play the game in your web browser on [the Tic-80 site](https://tic80.com/play?cart=2302),  
or play just the bare html game at  [http://composed.nu/alchemy/](http://composed.nu/alchemy/).

Source code is available on Github:  
[https://github.com/peterhil/alchemy](https://github.com/peterhil/alchemy)


## Development

This is a Lisp Game Jam [submission](https://itch.io/jam/autumn-lisp-game-jam-2021/rate/1248757) originally done in 10 days. I did not have time to do many things on version 0.1.0, like announce winning the game on reaching Keter.

The game is written in Fennel Lisp. Includes some obscure complex
mathematics and weird emanations of forms.

### Testing

Run tests with the `fennel` command:

    fennel spec.fnl


[alchemy]: https://en.wikipedia.org/wiki/Alchemy#Esoteric_interpretations_of_historical_texts
[Magnum Opus]: https://en.wikipedia.org/wiki/Magnum_opus_(alchemy)
[Tree of Life]: https://en.wikipedia.org/wiki/Tree_of_life_(Kabbalah)
[Kether]: https://en.wikipedia.org/wiki/Keter
[planets]: https://en.wikipedia.org/wiki/Alchemical_symbol#Seven_planetary_metals
[symbolical meanings]: https://www.crystalinks.com/birdsalchemy.html%E2%80%8B%E2%80%8B
[the ten holy sephiroth]: https://en.wikipedia.org/wiki/Sefirot
[sephiroth]: https://en.wikipedia.org/wiki/Sefirot
