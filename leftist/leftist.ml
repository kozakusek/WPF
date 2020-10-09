type 'a queue = 
	Node of 
		{value: 'a; 
		 left_son: 'a queue;
		 right_son: 'a queue;	
		 rheight: int; } (*minimalna odleglosc do liscia*)
    | Null;;

let (empty: 'a queue) = Null;;	

let is_empty (kolejeczka: 'a queue) = 
	if kolejeczka = empty then true
	else false;;

exception Empty;;
	 
let rec join (d1:'a queue) (d2: 'a queue) =
	match (d1,d2) with (*corner case'y*)
	| (Null,Null) -> Null
	| (Null,Node(sosna)) -> Node(sosna)
	| (Node(modrzew),Null) -> Node(modrzew)
	(* zwyczajny przypadek*)
	| (Node(modrzew),Node(sosna)) -> (
		if modrzew.value > sosna.value (*chcemy aby wartosc d1 < wartosc d2*)
			then join (Node(sosna)) (Node(modrzew))
		else
			let d3 = join modrzew.right_son (Node(sosna)) in
			match (modrzew.left_son,d3) with
			| (Null,Null) -> Null
			| (Null,Node(metasekwoja)) -> 
				Node(	
					{value = modrzew.value; 
					 left_son = (Node(metasekwoja));
					 right_son = Null; 
					 rheight = 0})
			| (Node(kopalina),Null) -> 
				Node(
					{value = modrzew.value; 
					 left_son = (Node(kopalina)); 
					 right_son = Null; 
					 rheight = 0})
			| (Node(kopalina),Node(metasekwoja)) -> (
				(*sprawdzamy warunek na lewicowosc*)
				if kopalina.rheight < metasekwoja.rheight then 
					Node(
						{value = modrzew.value; 
					     left_son = (Node(metasekwoja)); 
						 right_son = (Node(kopalina)); 
						 rheight = (kopalina.rheight + 1)})
				else
					Node(		
						{value = modrzew.value; 
						left_son = (Node(kopalina)); 
						right_son = (Node(metasekwoja)); 
						rheight = (metasekwoja.rheight + 1)})))
	
let add (listek: 'a) (drzewko: 'a queue) = 
	let (sadzonka: 'a queue) = (*tworzymy jednoelementowa kolejke*)
		Node 
		{value = listek;
		 left_son = empty;
		 right_son = empty;
		 rheight = 0;}
	in join sadzonka drzewko;;  (*laczymy je obie*)

let delete_min (stosik: 'a queue) = (*zwraca krotkę wartosci i merge'a dzieci*)
	match stosik with
  	| Null -> raise Empty
	| Node(stos) -> (stos.value, join stos.right_son stos.left_son);;    