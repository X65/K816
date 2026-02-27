use anyhow::Result;
use k816_o65::O65Object;
use std::collections::{HashMap, HashSet};

/// Reorder objects using breadth-first search starting from the object
/// that defines `main`. Objects not reachable through external symbol
/// references are dropped.
pub fn bfs_reorder(objects: Vec<O65Object>) -> Result<Vec<O65Object>> {
    if objects.is_empty() {
        return Ok(objects);
    }

    // Build index: symbol name -> object index (for defined symbols only)
    let mut symbol_to_obj: HashMap<&str, usize> = HashMap::new();
    for (idx, obj) in objects.iter().enumerate() {
        for sym in &obj.symbols {
            if sym.definition.is_some() {
                symbol_to_obj.insert(&sym.name, idx);
            }
        }
    }

    // Find the object containing "main"
    let main_idx = *symbol_to_obj
        .get("main")
        .ok_or_else(|| anyhow::anyhow!("no object defines 'main'"))?;

    // BFS traversal following external relocations
    let mut order: Vec<usize> = vec![main_idx];
    let mut visited: HashSet<usize> = HashSet::from([main_idx]);
    let mut queue_pos: usize = 0;

    while queue_pos < order.len() {
        let current = order[queue_pos];
        queue_pos += 1;

        let obj = &objects[current];

        // Symbols defined in this object (to skip local relocations)
        let local_symbols: HashSet<&str> = obj
            .symbols
            .iter()
            .filter(|s| s.definition.is_some())
            .map(|s| s.name.as_str())
            .collect();

        for reloc in &obj.relocations {
            if local_symbols.contains(reloc.symbol.as_str()) {
                continue;
            }
            if let Some(&provider_idx) = symbol_to_obj.get(reloc.symbol.as_str())
                && visited.insert(provider_idx) {
                    order.push(provider_idx);
                }
            // Symbol not in any object: may come from linker config, will be
            // checked during actual linking.
        }
    }

    // Collect objects in BFS order, dropping unreachable ones
    let mut indexed: Vec<Option<O65Object>> = objects.into_iter().map(Some).collect();
    let result = order
        .into_iter()
        .map(|i| indexed[i].take().expect("BFS index used twice"))
        .collect();

    Ok(result)
}
