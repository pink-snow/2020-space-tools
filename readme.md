# Annotate

![annotated/26.svg](https://pink-snow.github.io/2020-space-pages/annotated/26.svg)

This tool was used during the teaser stage of the [ICFP Programming Contest 2020] to annotate and decode bitmap messages received by [Pegovka observatory] and [Pflockingen Institute].
You can see the rendered result here ([annotated], [annotated-pf]) or at the official [message-from-space] site.

## Revisions
* `annotate.py` — the first version of the tool, now deprecated.
* `annotate.hs` — the actual version of the tool.
* `annotate-pf.hs` — this version is adapted to messages received by Pflockingen Institute.

To generate all images, run this command: `nix-shell --run make`.
This command would download the input data from the net, run the annotate tool, and put the generated output to `out/annotated` and `out/annotated-pf` directories.
Dependency: [Nix](https://nixos.org) package manager.

## Bugs
Message 15 (Send) and 35 (Modulate List) are annotated incorrectly.


# Deobfuscate

This tool made after the contest was used to translate `galaxy.txt` code into human readable code.
See my blog entry on [deobfuscating `galaxy.txt`] for more details.

# Decode animation

![decode-animation.gif](https://pink-snow.github.io/2020-space-pages/decode-animation.gif)

Animation based on the first radio transmission recording.

To generate this animation, run `nix-shell --run 'make out/anim/anim.gif'`.
Dependencies: `nix`, `cargo-scripter`.

The code is based on the code contributed by Discord user @aaaa1.


# Other stuff

This repository contains the code written before and after the contest.
You might want to check out [the repository of `codingteam`], in which I had the fortune to participate during the main phase of the contest.


[Pegovka observatory]: https://pegovka.space/
[Pflockingen Institute]: https://pflockingen.serveblog.net/unnatural-radio-signals-received-from-cygnus.html
[ICFP Programming Contest 2020]: https://icfpcontest2020.github.io/
[annotated]: https://pink-snow.github.io/2020-space-pages/annotated/
[annotated-pf]: https://pink-snow.github.io/2020-space-pages/annotated-pf/
[message-from-space]: https://message-from-space.readthedocs.io/
[deobfuscating `galaxy.txt`]: https://telegra.ph/Deobfuscating-galaxytxt-07-24
[the repository of `codingteam`]: https://github.com/codingteam/icfpc-2020
