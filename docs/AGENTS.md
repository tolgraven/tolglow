# Documentation and work tracking

Keep observed behavior, implemented changes, proposals and research clearly separate.
Link source evidence to the surveyed revision. `work-items.edn` is authoritative;
update status, prerequisites, ownership and evidence together. Run
`lein check-roadmap` from the repo root and check local Markdown links.
Document benchmark costs excluded from measurement. Changes to shared runtime,
control, registry and rig schemas need coordinated consumer review.

## Continuing a request

New steering adds scope unless the user cancels or replaces an objective. Append an entry to [the register](request-coverage.md) and link it to a work item with a concrete acceptance check. Preserve old request IDs when splitting or clarifying an item. Claim a disjoint scope when there is capacity. Otherwise leave it queued, with its dependencies and the last verified revision. A finished or inaccessible agent thread is a reason to check the branch, PR and evidence before updating status. Include a still-open list in handoffs.
