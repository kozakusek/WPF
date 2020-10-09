open PMap
exception Cykliczne
 
(*tworzy mape z grafu*)  
let make_map graph =
  let nodelist = (*lista samych wierzchołków*)
	let rec make_list graf acc= 
      match graf with 
      | [] -> List.rev acc
      | (h, _)::t -> make_list t (h::acc)
    in make_list graph [] in	 
  let nodegraph = (*mapa krotek (wierzcholek * [])*)
	let rec aux graf lista = 
      match lista with 
      | [] -> graf 
      | h::t -> aux (add h [] graf) t
    in aux empty nodelist in
  let rec finish grafik graf = (*wypełnienie pustch list w mapie*)
    match graf with 
      | [] -> grafik 
      | (v, tails)::t ->
        let comp = find v grafik
	      in finish (add v (comp@tails) grafik) t  
  in finish nodegraph graph	
	
(*dfs (wynik, PMap wyniku, PMap sciezki) (wierzcholek startowy) (mapa grafu)*)
let dfs (res, res_map, path) start mapa = 
  let rec t_sort (res, res_map, path) = function 
    | [] -> (res, res_map, path)
    | h::t -> 
	  if mem h path then (*jesli h na sciezce sprawdza czy zmpaowany*)
		if mem h res_map then (res, res_map, path) else raise Cykliczne
	  else let (new_res, new_res_map, new_path) = (*jesli nowy, to do dzieci*)
		List.fold_left (fun acc x -> t_sort acc [x]) 
		  (res, res_map, (add h 0 path)) (try find h mapa with Not_found ->[]) 
		in t_sort (h::new_res, add h 0 new_res_map, new_path) t
  in t_sort (res, res_map, path) [start]

let topol graph = 
  let mapa = make_map graph in 
  let(res,_,_)=List.fold_left(fun ac (x,_)->dfs ac x mapa)([],empty,empty)graph
  in res