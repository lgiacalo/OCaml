(*

La racine d'un arbre binaire est le nœud supérieur. 
Une feuille d'un arbre binaire est un nœud sans sous-arbre. 
La taille d'un arbre binaire est le nombre de nœuds définis. 
La hauteur d'un arbre binaire est le nombre de relations sur le chemin descendant le plus long entre la racine et une feuille

Écrivez une taille de fonction qui prend un arbre comme paramètre et renvoie sa taille. 
La fonction doit être saisie comme suit:
       val size: 'a tree -> int

Écrivez une hauteur de fonction qui prend un arbre comme paramètre et renvoie sa hauteur.
La fonction doit être saisie comme suit:
        val height: 'a tree -> int

Écrivez une fonction draw_tree qui prend un arbre comme paramètre et le dessine en utilisant le module graphique. 
Nous supposons que la taille de la fenêtre est suffisante pour la dessiner, vous n'avez donc pas à la gérer. 
Nous ne considérerons que les arbres de type arbre de chaînes pour cette fonction, vous pouvez donc dessiner les valeurs sans problème de conversion.
La fonction doit être saisie comme suit:
        val draw_tree: string tree -> unit
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


let main () = 
        Graphics.open_graph " 800x800";
        draw_square 700 700 80;
        draw_tree_node (Node ("AA", Node ("BB", Nil, Node ("CC", Node ("DD", Node ("EE", Nil, Nil), Nil), Node ("FF", Nil, Node ("GG", Nil, Nil)))), Nil));
        ignore (Graphics.read_key ());
        print_int (size (Node ("AA", Node ("BB", Nil, Nil), Nil)));
        print_char '\n';
        print_int (size (Node ("AA", Nil, Node ("BB", Nil, Nil))));
        print_char '\n';
        
        print_int (size (Node ("AA", Node ("BB", Nil, Node ("CC", Node ("DD", Node ("EE", Nil, Nil), Nil), Node ("FF", Nil, Node ("GG", Nil, Nil)))), Nil)));
        print_char '\n';
        print_int (height (Node ("AA", Node ("BB", Nil, Node ("CC", Node ("DD", Node ("EE", Nil, Nil), Nil), Node ("FF", Nil, Node ("GG", Nil, Nil)))), Nil)));
        print_char '\n';
        print_int (height (Node ("AA", Nil, Node ("BB", Nil, Node ("CC", Nil, Nil)))))

let () = main ()
