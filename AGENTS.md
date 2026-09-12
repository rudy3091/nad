# nad

`.claude/skills/nad-config/` restates the config surface, so it drifts silently
— nothing fails to build. Change `Config`, `Rule`, `Action`, the key table,
`LayoutSpec`, `Bar.Segment`, `Nad.hs`'s exports or the recompile path, and
update `SKILL.md` and `example.hs` in the same change. Then:

```sh
cabal exec -- ghc -fno-code .claude/skills/nad-config/example.hs
```

`cabal build` does not reach the running nad: that is
`~/.nad/nad-<arch>-darwin`, built against the *installed* library, and a stale
one looks just like a change that did not work. Run `scripts/bundle.sh`, then
have the user `nad restart`.
