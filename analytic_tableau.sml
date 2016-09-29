datatype Prop =  ATOM of string | NOT of Prop | AND of Prop * Prop | OR of Prop * Prop | IMP of Prop * Prop

fun search(x, []) = false
   |search(x,y::ys) = if(x = y) then true else search(x,ys)

fun reorder(ATOM(x), xs) = ATOM(x)::xs
	|reorder(NOT(ATOM(x)), xs) = NOT(ATOM(x))::xs
	|reorder(NOT(NOT(x)) ,xs) = NOT(NOT(x))::xs
	|reorder(AND(x,y), xs) = AND(x,y)::xs
	|reorder(NOT(OR(x,y)), xs) = NOT(OR(x,y))::xs
	|reorder(NOT(IMP(x,y)), xs) = NOT(IMP(x,y))::xs
	|reorder(OR(x,y), xs) = xs@[OR(x,y)]
	|reorder(NOT(AND(x,y)), xs) = xs@[NOT(AND(x,y))]
	|reorder(IMP(x,y), xs) = xs@[IMP(x,y)]

fun analytic_tableau ([],prop_list) = prop_list
   |analytic_tableau (AND(x,y)::xs, prop_list) = 
	let
		val temp = analytic_tableau(reorder(x,reorder(y,xs)),prop_list)
	in
		if temp = [] then [] else AND(x,y)::temp
	end
   |analytic_tableau (NOT(NOT(x))::xs, prop_list) = analytic_tableau(reorder(x,xs), prop_list)
   |analytic_tableau (NOT(OR(x,y))::xs, prop_list)  = 
	let
		val temp = analytic_tableau(NOT(x)::NOT(y)::xs , prop_list)
	in
		if (temp = []) then [] else NOT(OR(x,y))::temp
	end
   |analytic_tableau (NOT(IMP(x,y))::xs, prop_list) = 
	let
		val temp = analytic_tableau(NOT(y)::reorder(x,xs), prop_list) 
	in 
		if (temp = []) then [] else NOT(IMP(x,y))::temp
	end
   |analytic_tableau (OR(x,y) :: xs, prop_list) = 
	let 
		val left = analytic_tableau(reorder(x,xs), prop_list)
	in 
		if (left = []) then 
			let
				val right = analytic_tableau(reorder(y,xs), prop_list)
			in
				if (right = []) then [] else OR(x,y)::right 
			end
		else OR(x,y)::left
	end   
   |analytic_tableau (NOT(AND(x,y)) :: xs, prop_list) = 
	let
		val left = analytic_tableau(NOT(x)::xs, prop_list)
	in
		if (left = []) then
			let 
				val right = analytic_tableau(NOT(y)::xs, prop_list)
			in
				if (right = []) then [] else NOT(AND(x,y))::right
			end
		else NOT(AND(x,y)) :: left
	end
  |analytic_tableau (IMP(x,y) :: xs, prop_list) = 
	let
		val left = analytic_tableau(NOT(x) :: xs, prop_list)
	in
		if (left = []) then
			let
				val right = analytic_tableau(reorder(y, xs), prop_list)
			in
				if (right = []) then [] else IMP(x,y)::right
			end
		else IMP(x,y)::left
	end
 |analytic_tableau (ATOM(x)::xs, prop_list) = 
	if (search(NOT(ATOM(x)) , prop_list)) then [] else analytic_tableau(xs, ATOM(x)::prop_list)
 |analytic_tableau (NOT(ATOM(x)) :: xs, prop_list) = 
	if (search (ATOM(x) , prop_list)) then [] else analytic_tableau(xs, NOT(ATOM(x))::prop_list)
	
		

