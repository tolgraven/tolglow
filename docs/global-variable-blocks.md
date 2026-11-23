# Compose a shared variable chain

On the **Compose** page, open **Show variable chains**. Choose **Edit variable chains** and search for part of a keyword in **Find live variables**. The picker lists numeric values and vectors of two to four numbers. If a source will appear later, expand **Enter a variable path** and type its keys separated by spaces. For example, `weather density` addresses `[:variables :weather :density]`. A namespaced key such as `audio/energy` occupies one position in the path.

1. Choose **Live source** to start every frame with the latest input. Choose **Fixed value** to supply a number, vector or declarative expression. Set **Signal type** to match that value.
2. Pick a material or oscillator and tap **Add stage**. Each stage receives the previous stage’s output. Set the blend and wet amount, then adjust its inputs or reorder stages. Global variables have no cue parameters. A local prefab in the show’s variable-block settings is available alongside the built-in library.
3. Tap **Save variable chain**. The show owner validates and installs the entire binding together. The live plot then shows the dry input, selected stage changes and final result. The plot requests samples while visible and releases the subscription when scrolled away or closed. Changes stay in the edit form until saved.
4. To revise a binding later, open **Saved chains**. If another operator changes the same path, choose **Reload binding** to read the show’s version before making another edit. Saving a different variable preserves both operators’ changes. **Remove chain** restores direct access to the source value.

Every running effect that reads the shared variable sees the result. Saving the show includes both the source and the binding, so reopening it reconstructs the chain. For a private per-effect variable, author a binding on the effect definition. [Effect building blocks](effect-blocks.md)

## REPL and Ring

The REPL and the Studio route submit the same operation to the show owner. Pass the binding that you originally read as `:expected`, or `nil` for an unbound path. The owner returns `:stale-binding` if another editor has since written a different binding. Resubmitting a write already applied to that path succeeds.

```clojure
(show/submit! runtime :variable-block
              {:op :put
               :path [:variables :weather :density]
               :expected nil
               :binding {:stages [{:block :lfo/triangle :mode :multiply}]}})
```

A Ring client submits the same fields and the current runtime ID and expected epoch to `POST /api/variable-block` with Transit. To browse available sources, submit `{:query "density"}` to `POST /api/variable-block/sources`. Discovery examines up to 2,048 source-tree nodes and returns up to 48 matching paths and types. For the live plot, acquire `[:runtime/block-trace nil [:variables :weather :density]]` via the Studio WebSocket and release it when the view unmounts.
