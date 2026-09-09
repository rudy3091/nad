# nad

`.claude/skills/nad-config/` restates the config surface, so it drifts silently
— nothing fails to build. Change `Config`, `Rule`, `Action`, the key table,
`LayoutSpec`, `Bar.Segment`, `Nad.hs`'s exports or the recompile path, and
update `SKILL.md` and `example.hs` in the same change. Then:

```sh
cabal exec -- ghc -fno-code .claude/skills/nad-config/example.hs
```
