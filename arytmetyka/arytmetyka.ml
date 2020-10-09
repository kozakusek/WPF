type wartosc = (float * float * bool);;

let wartosc_dokladnosc x y = (x*.(1.0-.(y)/.100.0) , x*.(1.0+.(y)/.100.0), false);;

let wartosc_od_do x y = (x,y,false);;

let wartosc_dokladna x = (x,x,false);;

let max_wartosc (a,b,c) = 
	if c = false then b		
	else if b=infinity then a
		else infinity;;
		
let min_wartosc (a,b,c) = 
	if c  = false then a
	else if a = neg_infinity then b
		else neg_infinity;;
		
let in_wartosc (a,b,c) x = 
	if c=false then	
		if ((x>=a) && (x<=b)) then true 
		else false
	else if ((x<=a) || (x>=b)) then true
		else false;;

let sr_wartosc (a,b,c) = 
	if c = false then((a+.b)/.2.0)
	else nan;;	

let plus (a,b,c) (d,e,f) = 
	if (c = false && f = false)then ( (a+.d),(b+.e), false )
	else if (c = false && f = true) then 
		if (d+.b>=e+.a) then ( neg_infinity, infinity, false )
		else if  (e = infinity) then ( neg_infinity, (d+.b), false )
		else if (d = neg_infinity) then ( (e+.a), infinity, false) 
		else ( (d+.b),(e+.a), true )
	else if (c = true && f = false) then
		if (e+.a>=b+.d) then (neg_infinity, infinity, false)
		else if (b = infinity) then ( neg_infinity, (a+.e), false )
		else if  (a = neg_infinity) then ( (b+.d), infinity, false )
		else ( (e+.a),(b+.d), true )
	else 
		if (e = infinity && b = infinity && d != neg_infinity && a != neg_infinity) then ( neg_infinity, (a+.d), false)
		else if (e != infinity && b != infinity && d = neg_infinity && a = neg_infinity) then ( (b+.e), infinity, false)
		else (neg_infinity, infinity, false);;	
		
let minus (a,b,c) (d,e,f) =		
	if (c = false && f = false)then (( min (a-.d) (min (a-.e) (min (b-.d) (b-.e)))),(max (a-.d) (max(a-.e) (max (b-.d)(b-.e)))), false )
	else if (c = false && f = true) then 
		if  (e = infinity) then ( (a-.d)  , infinity, false )
		else if (d = neg_infinity) then ( neg_infinity, (b-.e), false ) 
		else ( neg_infinity,infinity, false )
	else if (c = true && f = false) then
		if (b = infinity) then ( neg_infinity, (a-.d), false )
		else if  (a = neg_infinity) then ( (b-.d), infinity, false )
		else if (a-.d>=b-.e) then ( neg_infinity, infinity, false )
		else ((a-.d),(b-.e),true)
	else 
		if (b = infinity) then
			if (d = neg_infinity) then (neg_infinity,(a-.e),false)
			else (neg_infinity,infinity,false)
		else if (a = neg_infinity) then 
			if (e = neg_infinity) then  ((b-.d),infinity,false)
			else (neg_infinity,infinity,false)
		else (neg_infinity, infinity, false);;
		
let razy (a,b,c) (d,e,f) = 
	if (c = false && f = false)then 
		if ((a=0.0 && b=0.0) || (d=0.0 && e=0.0)) then (0.0,0.0,false)	
		else (( min (a*.d) (min (a*.e) (min (b*.d) (b*.e)))),(max (a*.d) (max(a*.e) (max (b*.d)(b*.e)))), false )
	else if (c = false && f = true) then 
		if (a=0.0 && b=0.0 ) then (0.0,0.0,false)	
		else if (a<0.0 && b<=0.0) then
			if  (e = infinity) then ( (min (a*.d)(b*.d)), infinity, false ) 
			else if (d = neg_infinity) then ( neg_infinity , (max (e*.a)(e*.b)), false )
			else ( neg_infinity,infinity, false )
		else
			if  (e = infinity) then ( neg_infinity , (max (d*.a)(d*.b)), false )
			else if (d = neg_infinity) then ( (min (a*.e)(b*.e)), infinity, false ) 
			else ( neg_infinity,infinity, false )
	else if (c = true && f = false) then
		if (d=0.0 && e=0.0) then (0.0,0.0,false)	
		else if (d<0.0 && e<=0.0) then
			if  (b = infinity) then ( (min (a*.e)(a*.d)), infinity, false ) 
			else if (a = neg_infinity) then ( neg_infinity, (max (b*.d)(b*.e)), false ) 
			else ( neg_infinity,infinity, false )
		else	
			if (b = infinity) then ( neg_infinity, (max (a*.e)(a*.d)), false )
			else if  (a = neg_infinity) then ( (min (b*.d)(b*.e)), infinity, false )
			else (neg_infinity,infinity, false)
	else 
		(( min (a*.d) (min (a*.e) (min (b*.d) (b*.e)))),(max (a*.d) (max(a*.e) (max (b*.d)(b*.e)))),false);;

let podzielic (a,b,c) (d,e,f)= 
	let rec help (a,b,c) (d,e,f) =
		if (c = false && f = false)then 
			if (d=0.0 && e=0.0) then (nan,nan,false) (* raise exception*)
			else if (e>=0.0 && d<=0.0) then	
				if (a>=0.0 && b>=0.0) then ((a/.d),(a/.e),true)
				else if (a<=0.0 && b<=0.0) then ((b/.e),(b/.d),true)
				else ((max (a/.e)(b/.d)),(min(a/.d)(b/.e)),true)
			else ( (min (a/.d) (min (a/.e) (min (b/.d) (b/.e)))),(max (a/.d) (max(a/.e) (max (b/.d)(b/.e)))),false)
		else if (c = false && f = true) then
			if (f=true && d=neg_infinity && e=infinity) then (nan,nan,false) (* raise exception*)
			else if (a>=0.0 && b>=0.0)then
				if (e=infinity)then
					if (d=0.0)	then (neg_infinity,0.0,false)
					else if (d>0.0) then (0.0,(a/.d),true)
					else ((a/.d),0.0,false)
				else if (d=neg_infinity) then
					if(e>0.0) then (0.0, (b/.e), false)
					else if(e<0.0) then  ((a/.e),0.0,true)
					else (0.0,infinity,false)
				else
					if (e<0.0 || d>0.0) then (neg_infinity,infinity,false)
					else ((b/.d),(b/.e),false)
			else if (a<=0.0 && b<=0.0)then
				if (e=infinity)then
					if (d=0.0)	then (0.0,infinity,false)
					else if (d>0.0) then ((b/.d),0.0,true)
					else (0.0,(a/.d),false)
				else if (d=neg_infinity)then
					if(e>0.0) then ((a/.e),0.0,  false)
					else if(e<0.0) then  (0.0,(b/.e),true)
					else (neg_infinity,0.0,false)
				else
					if (e<0.0 || d>0.0) then (neg_infinity,infinity,false)
					else ((a/.e),(a/.d),false)
			else
				if (e=infinity) then
					if (d<0.0) then ((b/.d),(a/.d),false)
					else (neg_infinity,infinity,false)
				else if (d=neg_infinity) then
					if (e>0.0) then ((a/.e),(b/.e),false)
					else (neg_infinity,infinity,false)
				else
					if (e<0.0 || d>0.0) then (neg_infinity,infinity,false)
					else ((min(b/.d)(a/.e)),(max(a/.d)(b/.e)),false)
		else if (c = true && f = false) then
			if (d=0.0 && e=0.0) then (nan,nan,false) (* raise exception*)
			else if (b=infinity) then
				if (d>=0.0 && e>=0.0)then ((neg_infinity),(a/.d),false)
				else if (d<=0.0 && e<=0.0)then ((min (a/.e)(a/.d)),infinity,false)
				else 
					if (a=0.0) then (neg_infinity,infinity,false)
					else if (a>0.0) then ((a/.d),(a/.e),true)
					else ((a/.e),(a/.d),true)
			else if (a=neg_infinity) then
				if (d>=0.0 && e>=0.0)then ((min(b/.e)(b/.d)),infinity,false)
				else if (d<=0.0 && e<=0.0)then (neg_infinity,(max(b/.d)(b/.e)),false)
				else
					if (b=0.0) then (neg_infinity,infinity,false)
					else ((b/.e),(b/.d),true)
			else 
				if (d>=0.0 && e>=0.0)then
					if (a<0.0 && b>0.0) then ((a/.e),(b/.e),true)
					else if  (a>=0.0 && b>=0.0) then 
						if ((a/.d)>=(b/.e)) then  (neg_infinity,infinity,false)
						else ((a/.d),(b/.e),true)
					else
						if ((a/.e)>=(b/.d)) then (neg_infinity,infinity,false)
						else ((a/.e),(b/.d),true)
				else if (d<=0.0 && e<=0.0)then
					if (a<0.0 && b>0.0) then ((b/.e),(a/.e),true)
					else if (a>=0.0 && b>=0.0) then 
						if ((b/.d)>=(a/.e)) then  (neg_infinity,infinity,false)
						else ((b/.d),(a/.e),true)
					else
						if ((b/.e)>=(a/.d)) then (neg_infinity,infinity,false)
						else ((b/.e),(a/.d),true)
				else
					if (a<0.0 && b>0.0) then 
						if ((max (a/.e)(b/.d))>=(min(b/.e)(a/.d))) then (neg_infinity,infinity,false)
						else ((max (a/.e)(b/.d)),(min(b/.e)(a/.d)),true)
					else if(a>=0.0 && b>=0.0) then 
						if ((max (a/.d)(b/.d))>=(min(b/.e)(a/.e))) then (neg_infinity,infinity,false)
						else ((max (a/.d)(b/.d)),(min(b/.e)(a/.e)),true)
					else
						if ((max (a/.e)(b/.e))>=(min(b/.d)(a/.d))) then (neg_infinity,infinity,false)
						else ((max (a/.e)(b/.e)),(min(b/.d)(a/.d)),true)

		else 
			if (b=infinity) then (help (neg_infinity,a,false) (d,e,true))
			else if (a=neg_infinity) then (help (b,infinity,false)(d,e,true))
			else if (e=infinity) then (help(a,b,true)(neg_infinity,d,false))
			else if (d=neg_infinity) then (help(a,b,true)(e,infinity,false))
			else 
				let (i,j,k) = help (neg_infinity,a,false) (d,e,true)
				and (m,n,o) = help (b,infinity,false) (d,e,true)
				in
					if (k=false && o=false) then ((min(i)(m)),(max(j)(n)),false)
					else if (k=true && o=false) then 
						if (m<=i && n>=j) then (neg_infinity,infinity,false)
						else if (i<m && j>n) then (nan,nan,true)	(*exception*)
						else if (m>i && n>=j) then (i,(min m j),true)
						else ((max i n),j,true)
					else if (k=false && o=true) then
						if (m<i && n>j) then (nan,nan,true)	(*exception*)
						else if (i<=m && j>=n) then (neg_infinity,infinity,false)
						else if (m>i && n>=j) then ((max m j),n,true)
						else (m,(min n i),true)
					else
						if (i>=n || j<=m) then (neg_infinity,infinity,false)
						else if (i>=m && j<=n) then (i,j,k)
						else if (i<=m && j>=n) then (m,n,o)
						else if (i>=m && i<n && j>n) then (i,n,true)
						else (m,j,true)
	in help (a,b,c) (d,e,f);;
	
(*testy*)	
let is_nan x = compare x nan = 0;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let c = plus a a
let d = razy a a
let e = podzielic a a
let f = minus a a;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));
assert ((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (* nan *)
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; (* <-0.1, 2.1> *)
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  (* <-3.0, 0.0> *)
let b = wartosc_od_do 0. 1.      (* <-0.0, 1.0> *)
let c = podzielic a b;;          (* (-inf, 0> *)
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

(* uwaga, ten test moze sie zawiesic przy pewnych implementacjach! *)
let a = wartosc_od_do (-2.) 3.
let b = wartosc_od_do 2. 3.
let c = podzielic b a

let rec iteruj f n acc = match n with
    | 0 -> acc
    | n when n > 0 -> iteruj f (n-1) (f acc acc)
    | _ -> acc

let x = iteruj razy 10 c;;
assert (not (in_wartosc x 0.));;	
	
	
	
	