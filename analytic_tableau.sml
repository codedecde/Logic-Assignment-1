Control.Print.printDepth := 100;
datatype Prop =  ATOM of string | NOT of Prop | AND of Prop * Prop | OR of Prop * Prop | IMP of Prop * Prop

fun search(x, []) = false
   |search(x,y::ys) = if(x = y) then true else search(x,ys)

fun reorder(x : Prop list): Prop list =
	let 
		fun reorder_helper ([]) = ([],[])
			|reorder_helper (ATOM(x)::xs ) = 
				let val (v1,v2) = reorder_helper(xs) in (ATOM(x)::v1,v2) end
			|reorder_helper (NOT(ATOM(x))::xs) = 
				let val (v1,v2) = reorder_helper(xs) in (NOT(ATOM(x))::v1 , v2) end
			|reorder_helper (AND(x,y)::xs) =
				let val (v1,v2) = reorder_helper(xs) in (AND(x,y)::v1 ,v2) end
			|reorder_helper (OR(x,y) :: xs) = 
				let val (v1,v2) = reorder_helper (xs) in (v1, OR(x,y)::v2) end
			|reorder_helper (IMP(x,y) :: xs) = 				  
				let val (v1,v2) = reorder_helper (xs) in (v1, IMP(x,y)::v2) end
			|reorder_helper (NOT(AND(x,y))::xs) = 
				let val (v1,v2) = reorder_helper (xs) in (v1, NOT(AND(x,y))::v2) end
			|reorder_helper (NOT(OR(x,y))::xs) = 		
				let val (v1,v2) = reorder_helper(xs) in (NOT(OR(x,y))::v1 ,v2) end
			|reorder_helper (NOT(IMP(x,y)) :: xs) = 
				let val (v1,v2) = reorder_helper(xs) in (NOT(IMP(x,y))::v1 ,v2) end
			|reorder_helper (NOT(NOT(x))::xs ) = 
				let val (v1,v2) = reorder_helper(xs) in (NOT(NOT(x))::v1 ,v2) end
			val (v1,v2) = reorder_helper(x)	
	in
		v1@v2
	end

fun analytic_tableau ([],atom_list) = atom_list
   |analytic_tableau (AND(x,y)::xs, atom_list) = 
	let
		val temp = analytic_tableau(reorder(x::y::xs),atom_list)
	in
		if temp = [] then [] else AND(x,y)::temp
	end
   |analytic_tableau (NOT(NOT(x))::xs, atom_list) = analytic_tableau(x::xs, atom_list)
   |analytic_tableau (NOT(OR(x,y))::xs, atom_list)  = 
	let
		val temp = analytic_tableau(reorder(NOT(x)::NOT(y)::xs ), atom_list)
	in
		if (temp = []) then [] else NOT(OR(x,y))::temp
	end
   |analytic_tableau (NOT(IMP(x,y))::xs, atom_list) = 
	let
		val temp = analytic_tableau(reorder(x::NOT(y)::xs), atom_list) 
	in 
		if (temp = []) then [] else NOT(IMP(x,y))::temp
	end
   |analytic_tableau (OR(x,y) :: xs, atom_list) = 
	let 
		val left = analytic_tableau(reorder(x::xs), atom_list)
	in 
		if (left = []) then 
			let
				val right = analytic_tableau(reorder(y::xs), atom_list)
			in
				if (right = []) then [] else OR(x,y)::right 
			end
		else OR(x,y)::left
	end   
   |analytic_tableau (NOT(AND(x,y)) :: xs, atom_list) = 
	let
		val left = analytic_tableau(reorder(NOT(x)::xs), atom_list)
	in
		if (left = []) then
			let 
				val right = analytic_tableau(reorder(NOT(y)::xs), atom_list)
			in
				if (right = []) then [] else NOT(AND(x,y))::right
			end
		else NOT(AND(x,y)) :: left
	end
  |analytic_tableau (IMP(x,y) :: xs, atom_list) = 
	let
		val left = analytic_tableau(reorder(NOT(x) :: xs), atom_list)
	in
		if (left = []) then
			let
				val right = analytic_tableau(reorder(y :: xs), atom_list)
			in
				if (right = []) then [] else IMP(x,y)::right
			end
		else IMP(x,y)::left
	end
 |analytic_tableau (ATOM(x)::xs, atom_list) = 
	if (search(NOT(ATOM(x)) , atom_list)) then [] else analytic_tableau(reorder(xs), ATOM(x)::atom_list)
 |analytic_tableau (NOT(ATOM(x)) :: xs, atom_list) = 
	if (search (ATOM(x) , atom_list)) then [] else analytic_tableau(reorder(xs), NOT(ATOM(x))::atom_list)
	
		

