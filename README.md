# My Emacs config

## Built from Emacs Literate Starter

Found here: <https://github.com/gilbertw1/emacs-literate-starter.git>.

## Usage

Install

```sh
git clone https://github.com/haritkapadia/emacs-config.git ~/.emacs.d
```

Compile (not required, but recommended)

```sh
cd ~/.emacs.d
make compile
```

Run

```sh
emacs
```


## Make Commands

**clean**: Delete compiled files

```sh
make clean
```

**compile**: Byte compile for performance (Recompile required when new changes are made)

```sh
make compile
```

## Notes

- Configuration can be run without being byte compiled first, but will load slower as a result.
- If configuration has been byte compiled then recompilation is required to pick up new config changes.

## Sources

A majority of the optimizations used in this config were sourced from:

- https://github.com/hlissner/doom-emacs
- https://github.com/nilcons/emacs-use-package-fast
- https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
