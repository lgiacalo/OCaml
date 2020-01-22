
(*
Écrivez une fonction draw_square qui prend trois paramètres x, y, size comme paramètres 
et tracez un carré centré en (x, y) sur une fenêtre graphique.
Vous devrez utiliser le module Graphics, dont les seules fonctions autorisées sont lineto, moveto et les fonctions draw_string.

Écrivez une fonction draw_tree_node qui prend un nœud d'arbre de base Node (v, Nil, Nil) ou Nil comme paramètre
et dessinez-le selon cet exemple (si vous préférez le dessiner de haut en bas, c'est ak) 

valeur moveto: int -> int -> unité
        Positionnez le point actuel.

valeur lineto: int -> int -> unit
        Tracez une ligne avec des extrémités le point actuel et le point donné et déplacez le point actuel vers le point donné.

valeur draw_string: string -> unit
Dessinez un caractère ou une chaîne de caractères avec le coin inférieur gauche à la position actuelle. Après le dessin, la position actuelle est définie dans le coin inférieur droit du texte dessiné.

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

        
let draw_tree_node node xx yy = 
        let size = 40 in
        let rec loop node x y = match node with
            | Nil -> (draw_square_string x y size "Nil")
            | Node (v, l, r) -> (
                    draw_square_string x y size (string_of_int v);
                    Graphics.moveto (x + (size / 2)) y;
                    Graphics.lineto (x + (size * 2) - 10) (y + size - 10);
                    loop l (x + (size * 2) - 10 + (size / 2)) (y + size - 10);
                    Graphics.moveto (x + (size / 2)) y;
                    Graphics.lineto (x + (size * 2) - 10) (y - size + 10);
                    loop r (x + (size * 2) - 10 + (size  / 2)) (y - size + 10)
            )
        in
        loop node xx yy


let main () = 
        Graphics.open_graph " 800x800";
        draw_square 700 700 80;
        draw_tree_node (Node (42, Nil, Nil)) 50 400;
        draw_tree_node Nil 50 600;
        ignore (Graphics.read_key ())

let () = main ()
