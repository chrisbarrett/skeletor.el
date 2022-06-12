## Pull-request tips

* There’s plenty of room for improvement in the code, because every
line of it has been written against the backdrop of ignorance and
fallibility, mostly my own. (= Principle of Prior Ignorance)

* PRs for simple documentation fixes (e.g., spelling and grammar
  corrections) are always welcome. For more substantial changes, I
  don’t necessarily prefer PRs to issues or feature requests. A good
  description of the problem with a working example is better than a
  half-baked PR. I can often fix it in less time than it would take to
  review the PR. (= Principle of Efficiency)

* Small PRs are easier to accept than large ones. Large PRs should
  have a benefit worthy of their complexity.

* PRs should be necessary, in the sense that the proposed change can
  only be accomplished by patching this repo. (Corollary: features
  that can live in a separate [package](https://pkgs.racket-lang.org/)
  probably should.) (= Principle of Necessity)

* PRs should forbid as little as possible. In particular, PRs should
  avoid enshrining personal preference as default behavior (because
  others will have different preferences). (= Principle of Generality)

* PRs should avoid reinventing features that already exist in
  Racket. (= Principle of Economy)

* PRs should fix real problems that have arisen in actual use, not
  theoretical or conjectural problems. (= Principle of Practical
  Justification)

* If your PR includes open-source material from elsewhere, please make
  sure that material is a) compatible with the license of this repo
  and b) attributed in whatever way is required. Otherwise, I cannot
  accept it. (= Principle of Legality)

* PRs that could have unit tests, and don’t, will be treated
  harshly. As they should. (= Principle of Proof)
