Control.Print.printDepth := 100;
datatype Prop =  ATOM of string | NOT of Prop | AND of Prop * Prop | OR of Prop * Prop | IMP of Prop * Prop

fun analytic_tableau (gamma, phi) = 
	let 
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
		
		fun analytic_tableau_helper ([],atom_list) = atom_list
		   |analytic_tableau_helper (AND(x,y)::xs, atom_list) = 
			let
				val temp = analytic_tableau_helper(reorder(x,reorder(y,xs)),atom_list)
			in
				if temp = [] then [] else AND(x,y)::temp
			end
		   |analytic_tableau_helper (NOT(NOT(x))::xs, atom_list) = analytic_tableau_helper(reorder(x,xs), atom_list)
		   |analytic_tableau_helper (NOT(OR(x,y))::xs, atom_list)  = 
			let
				val temp = analytic_tableau_helper(NOT(x)::NOT(y)::xs , atom_list)
			in
				if (temp = []) then [] else NOT(OR(x,y))::temp
			end
		   |analytic_tableau_helper (NOT(IMP(x,y))::xs, atom_list) = 
			let
				val temp = analytic_tableau_helper(NOT(y)::reorder(x,xs), atom_list) 
			in 
				if (temp = []) then [] else NOT(IMP(x,y))::temp
			end
		   |analytic_tableau_helper (OR(x,y) :: xs, atom_list) = 
			let 
				val left = analytic_tableau_helper(reorder(x,xs), atom_list)
			in 
				if (left = []) then 
					let
						val right = analytic_tableau_helper(reorder(y,xs), atom_list)
					in
						if (right = []) then [] else OR(x,y)::right 
					end
				else OR(x,y)::left
			end   
		   |analytic_tableau_helper (NOT(AND(x,y)) :: xs, atom_list) = 
			let
				val left = analytic_tableau_helper(NOT(x)::xs, atom_list)
			in
				if (left = []) then
					let 
						val right = analytic_tableau_helper(NOT(y)::xs, atom_list)
					in
						if (right = []) then [] else NOT(AND(x,y))::right
					end
				else NOT(AND(x,y)) :: left
			end
		  |analytic_tableau_helper (IMP(x,y) :: xs, atom_list) = 
			let
				val left = analytic_tableau_helper(NOT(x) :: xs, atom_list)
			in
				if (left = []) then
					let
						val right = analytic_tableau_helper(reorder(y, xs), atom_list)
					in
						if (right = []) then [] else IMP(x,y)::right
					end
				else IMP(x,y)::left
			end
		 |analytic_tableau_helper (ATOM(x)::xs, atom_list) = 
			if (search(NOT(ATOM(x)) , atom_list)) then [] else analytic_tableau_helper(xs, ATOM(x)::atom_list)
		 |analytic_tableau_helper (NOT(ATOM(x)) :: xs, atom_list) = 
			if (search (ATOM(x) , atom_list)) then [] else analytic_tableau_helper(xs, NOT(ATOM(x))::atom_list)
	in
		analytic_tableau_helper(NOT(phi)::gamma , [])
	end
		

