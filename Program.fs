// For more information see https://aka.ms/fsharp-console-apps
open System

type 't btree =
    | Node of 't * 't btree * 't btree
    | Nil

let CTree () =
    let infix root left right = (left(); root(); right())

    /// вывод в консоль дерева
    let iterh trav f t =
        let rec tr t h =
            match t with
            | Node (x, L, R) ->
                trav
                    (fun () -> f x h)
                    (fun () -> tr L (h+1))
                    (fun () -> tr R (h+1))
            | Nil -> ()
        tr t 0

    let spaces n = List.fold (fun s _ -> s+"    ") "" [0..n]
    let print_tree T = iterh infix (fun x h -> printfn "%s%A" (spaces h) x) T

    /// вставка элемента в дерево
    let rec insert t x =
        match t with
        | Nil -> Node(x, Nil, Nil)
        | Node(z, L, R) ->
            if x < z then Node(z, insert L x, R)
            else Node(z, L, insert R x)

    let A =
        [
            let r = new Random()
            printfn "Количество элементов?"
            let n = Console.ReadLine() |> Convert.ToInt32
            for i in 1..n do
                yield r.Next(-150, 160)
        ]

    printfn "Исходный список %A" A
    let BT = A |> List.fold insert Nil
    print_tree BT

    /// Задание 1: Дублирование строк
    let rec map_tree f tree =
        match tree with
        | Nil -> Nil
        | Node(x, left, right) ->
            let new_x = f x
            Node(new_x, map_tree f left, map_tree f right)

    let duplicate_string s = s.ToString() + s.ToString()
    let new_tree = map_tree duplicate_string BT
    printfn "Новое дерево с дублированными строками:"
    print_tree new_tree

    /// Задание 2: Проверка наличия элемента
    let contains tree value =
        let rec check t =
            match t with
            | Nil -> false
            | Node(x, left, right) ->
                if x = value then true
                else check left || check right
        check tree

    printfn "Введите значение для поиска:"
    let search_value = Console.ReadLine() |> int
    let is_present = contains BT search_value
    printfn "Элемент %d %s" search_value (if is_present then "найден" else "не найден")

[<EntryPoint>]
let main args =
    CTree()
    0


