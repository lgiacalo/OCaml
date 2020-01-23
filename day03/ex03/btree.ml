(*
Ok, assez de plaisir, retour aux arbres. Un arbre de recherche binaire (BST) est un arbre binaire où les nœuds sont triés de manière à ce que pour chaque nœud, 
tous les nœuds du sous-arbre gauche de la racine soient inférieurs au nœud actuel et tous les nœuds du sous-arbre droit du root sont supérieurs au nœud actuel. 

Un arbre parfait est un arbre où toutes les feuilles ont 0 ou 2 fils et toutes les feuilles sont à égale distance de la racine.

• Écrivez une fonction is_bst qui prend un arbre comme paramètre et retourne un booléen.
Vrai si l'arbre est un bst, faux sinon.

• Écrivez une fonction is_perfect qui prend un arbre comme paramètre et retourne un booléen.
Vrai si l'arbre est parfait BST, faux sinon.

• Écrire une fonction is_balanced qui prend un arbre comme paramètre et retourne un booléen. Vrai si l'arbre est un BST à hauteur équilibrée, faux sinon.

• Écrivez une fonction search_bst qui prend une valeur et un BST comme paramètre et retourne un booléen. Vrai si la valeur est dans l'arborescence, faux sinon.

• Écrivez une fonction add_bst qui prend une valeur et un BST comme paramètre et retourne BST avec la nouvelle valeur insérée.

• Écrivez une fonction delete_bst qui prend un BST comme paramètre et une valeur et retourne un BST avec la valeur supprimée.

Vous pouvez rencontrer un cas dans lequel l'une de vos fonctions ne peut pas retourner une valeur car cela n'aurait aucun sens. Dans ce cas, au lieu de renvoyer une valeur, 
vous devez utiliser l'expression suivante: echec avec "votre message d'erreur ici", avec votre message d'erreur personnalisé obvioulsy.


 *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size = 
        let xx = (x + (size / 2)) in
        let yy = (y + (size / 2)) in
        Graphics.moveto xx yy;
        Graphics.lineto (xx - size) yy;
        Graphics.lineto (xx - size) (yy - size);
        Graphics.lineto xx (yy - size);
        Graphics.lineto xx yy

let draw_square_string x y size str = 
        draw_square x y size;
        Graphics.moveto (x - 6) (y - 5);
        Graphics.draw_string str
        
let draw_tree_node node = 
        let size = 40 in
        let x = 50 in 
        let y = 400 in
        let rec loop node x y = match node with
            | Nil -> (draw_square_string x y size "Nil")
            | Node (v, l, r) -> (
                    draw_square_string x y size (v);
                    Graphics.moveto (x + (size / 2)) y;
                    Graphics.lineto (x + (size * 2) - 10) (y + size - 10);
                    loop l (x + (size * 2) - 10 + (size / 2)) (y + size - 10);
                    Graphics.moveto (x + (size / 2)) y;
                    Graphics.lineto (x + (size * 2) - 10) (y - size + 10);
                    loop r (x + (size * 2) - 10 + (size  / 2)) (y - size + 10)
            )
        in
        loop node x y


let size node = match node with
        | Nil -> 0
        | _ -> (
                let rec loop arb acc = match arb with
                        | Nil -> acc
                        | Node (_, l, r) -> (
                                loop r ((loop l (acc + 1)))
                        )
                in 
                loop node 0
        )

let rec height node = match node with
        | Nil -> 0
        | Node (_, l, r) -> 1 + (max (height l) (height r))

(**************************************************************************************************************************************************************)

let rec is_btree node = match node with
        | Nil -> true
        | Node (y, Node (x, _, _), _) when (y < x) -> false
        | Node (y, _, Node (z, _, _)) when (y > z) -> false
        | Node (_, l, r) -> (is_btree l) && (is_btree r)

let rec is_balanced node = match node with
        | Nil -> true
        | Node (_, l, r) -> (
                let hl = (height l) in
                let hr = (height r) in
                if (((hl - hr) > 1) || ((hl - hr) < -1))
                then false
                else
                        (is_balanced l) && (is_balanced r)
        )

let rec is_perfect node = match node with
        | Nil -> true
        | Node (_, l, r) when ((height l) - (height r) <> 0) -> false
        | Node (_, l, r) -> (is_balanced l) && (is_balanced r)

let rec search_bst value node = match node with
        | Nil -> false
        | Node (x, _, _) when (value = x) -> true
        | Node (_, l, r) -> (search_bst value l) || (search_bst value r)

let rec add_bst value node = match node with
        | Node (x, l, r) when (value < x) -> (Node (x, (add_bst value l), r))
        | Node (x, l, r) when (value >= x) -> (Node (x, l, (add_bst value r)))
        | _ -> Node (value, Nil, Nil)
        
(**************************************************************************************************************************************************************)

let answer b =
        if b
        then 
                print_string "true\n"
        else
                print_string "false\n"

        
let main () = 
(*
        Graphics.open_graph " 800x800";
        draw_square 700 700 80;
        draw_tree_node (Node ("AA", Node ("BB", Nil, Node ("CC", Node ("DD", Node ("EE", Nil, Nil), Nil), Node ("FF", Nil, Node ("GG", Nil, Nil)))), Nil));
        ignore (Graphics.read_key ());
*)
        let btree_one = Node (10, Nil, Nil) in
        let btree_two_left = Node (10, Node (5, Nil, Nil), Nil) in
        let btree_two_leftt = Node (10, Node (15, Nil, Nil), Nil) in
        let btree_two_right = Node (10, Nil, Node (5, Nil, Nil)) in
        let btree_two = Node (10, Node (5, Nil, Nil), Node (15, Nil, Nil)) in
        let btree_a = Node ("N", Node ("H", Nil, Node ("K", Node ("J", Node ("I", Nil, Nil), Nil), Node ("L", Nil, Node ("M", Nil, Nil)))), Nil) in
        
        print_int (size btree_two_left);
        print_char '\n';
        print_int (size btree_two_right);
        print_char '\n';
        
        print_int (size btree_a); 
        print_char '\n';
        print_int (height btree_a);
        print_char '\n';
        print_int (height (Node ("AA", Nil, Node ("BB", Nil, Node ("CC", Nil, Nil)))));
        print_char '\n';

        print_endline "\nIs_btree :";
        answer (is_btree btree_one);
        answer (is_btree btree_two_left);
        answer (is_btree btree_two_leftt);
        answer (is_btree btree_two);
        answer (is_btree btree_a);

        print_endline "\nIs_balanced :";
        answer (is_balanced btree_one);
        answer (is_balanced btree_two_left);
        answer (is_balanced btree_two_leftt);
        answer (is_balanced btree_two);
        answer (is_balanced btree_a);

        print_endline "\nIs_perfect :";
        answer (is_perfect btree_one);
        answer (is_perfect btree_two_left);
        answer (is_perfect btree_two_leftt);
        answer (is_perfect btree_two);
        answer (is_perfect btree_a);

        print_endline "\nSearch_bst :";
        answer (search_bst 10 btree_one);
        answer (search_bst 20 btree_two_left);
        answer (search_bst 5 btree_two_leftt);
        answer (search_bst 15 btree_two);
        answer (search_bst "M" btree_a);
        answer (search_bst "I" btree_a);
        answer (search_bst "T" btree_a);

        print_endline "\nAdd_bst :";
        let bst_two_l =  (add_bst 5 btree_one) in
        answer (search_bst 5 bst_two_l);
        answer (is_perfect (bst_two_l));
        answer (is_perfect (add_bst 15 bst_two_l));






        print_endline "End"

let () = main ()







