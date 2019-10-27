///An implementation of frequent pattern growth algorithm to find frequent itemsets
///(generates only frequent itemsets; does not mine association rules)
//This is used for local community detection in graphs
module FPGrowth

open System.Collections.Generic

type Node<'i> = 
  {
    Item:'i
    mutable Support: int; 
    Children:Dictionary<'i,Node<'i>>
  }

type Root<'i> = {Heads:Dictionary<'i,Node<'i>>}
type TableEntry<'i>= {Item:'i; Frequency:int}
type Tree<'i> = {Root:Root<'i>}
type Path<'i> = {Items:'i list; Support:int}

let private yourself x = x

//scan transactions and find frequency of each distinct item
//sort results in descending order of frequency
let initTable (transactions:seq<'id*Set<'item>>) =
  let freq = transactions |> Seq.collect snd |> Seq.countBy yourself
  let table = 
    freq 
    |> Seq.map (fun (i,c) -> {Item=i; Frequency=c})
    |> Seq.sortBy (fun te -> -te.Frequency)
    |> Seq.toArray
  table

//sort items in each *transaction* by descending order of (global) item frequency (calculated in initTable)
let sortTransactions (transactions:seq<'id*Set<'item>>)  (table:TableEntry<'item>[]) =
  let tblIdx = table |> Seq.map (fun te -> te.Item, te ) |> dict
  let sorted = 
    transactions 
    |> Seq.map (fun (id,s)->
      id,
      s 
      |> Seq.filter (fun item -> tblIdx.ContainsKey item) 
      |> Seq.sortByDescending (fun item -> tblIdx.[item].Frequency) 
      |> Seq.toArray) 
    |> Seq.toArray
  sorted

//insert item (should be the first item of each transaction) into the root of the tree
let insertRoot (r:Root<_>) item =
    let node = 
      match r.Heads.TryGetValue item with
      | true,n -> n
      | false,_ -> 
        let n = {Item=item; Support=0; Children=Dictionary()}
        r.Heads.Add(item,n)
        n
    node.Support <- node.Support + 1     //count how times this item shows up as the first element of sorted transactions
    node
  
//insert a transaction item into the tree
let insertNode (r:Node<_>) item =
    let node = 
      match r.Children.TryGetValue item with
      | true,n -> n
      | false,_ -> 
        let n = {Item=item; Support=0; Children=Dictionary()}
        r.Children.Add(item,n)
        n
    node.Support <- node.Support + 1  //count how times this item shows in association with other items along the path to the root
    node

//recursively insert items of a (sorted) transaction into the tree
let rec insert acc tree (items:array<_>) i =
  if i >= items.Length then 
    acc
  else
    let item = items.[i]
    let n = 
      if i = 0 then 
        insertRoot tree.Root item
      else
        let prev = List.head acc
        insertNode prev item
    insert (n::acc) tree items (i+1)
      
let makeTree (transactions:seq<'id*Set<'item>>) =
  let table = initTable transactions
  let sorted = sortTransactions transactions table
  let tree = {Root={Heads=Dictionary()}}
  let _ = (tree,sorted) ||> Array.fold (fun tree (_,sortedTxns) -> 
    let _ = insert [] tree sortedTxns 0
    tree
  )
  tree

let rec mineNode acc (node:Node<_>) =
  if node.Children.Count = 0 then     //leaf
    seq{yield {Items=acc; Support=node.Support}}
  else
    let acc = node.Item::acc //combine path items
    node.Children |> Seq.collect (fun kv->mineNode acc kv.Value) //PSeq seems to be much slower for test data
  
//http://www.fssnip.net/2z/title/All-combinations-of-list-elements
let allCombinations lst =
    let rec comb accLst elemLst =
        match elemLst with
        | h::t ->
            let next = [h]::List.map (fun el -> h::el) accLst @ accLst
            comb next t
        | _ -> accLst
    comb [] lst

let mineTree support (tree:Tree<_>) =
  let paths = tree.Root.Heads |> Seq.collect (fun kv->mineNode [] kv.Value) //find support for each path from root to leaf
  paths |> Seq.collect (fun {Items=is;Support=c} ->
    let combos = allCombinations is |> List.map set         //explode all possible combinations of items in the path
    combos |> List.map (fun combo -> combo,c))              //associate them with support from this path
  |> Seq.filter (fun (combo,_) -> Set.isEmpty combo |> not) //filter out empty sets
  |> Seq.groupBy fst                                        //group by each combination of items generated earlier
  |> Seq.map (fun (k,vs)->k,vs|>Seq.sumBy snd)              //sum up individual supports for each group into total support
  |> Seq.filter (fun (_,s)-> s >= support)                  //filter out combinations that fall below the specified support threshold

(* usage  

#load "FPGrowth.fs"
open FPGrowth
let transactions = //each character in the strings below is a transaction item
  [|"abcefo"; "acg"; "ei"; "acdeg"; "acegl"; "ej"; "abcefp"; "acd"; "acegm"; "acegn"|]
  |> Array.mapi (fun i s-> i,s |> Seq.toList |> set)

let support = 3
let tree = makeTree transactions //make a compressed representation of the transactions
let frequentItems = mineTree support tree |> Seq.toList

>
val frequentItems : (Set<char> * int) list =
  [(set ['a'], 8); (set ['a'; 'c'], 8); (set ['a'; 'c'; 'e'], 6);
   (set ['a'; 'e'], 6); (set ['c'], 8); (set ['c'; 'e'], 6); (set ['e'], 8);
   (set ['a'; 'c'; 'e'; 'g'], 4); (set ['a'; 'c'; 'g'], 4);
   (set ['a'; 'e'; 'g'], 4); (set ['a'; 'g'], 4); (set ['c'; 'e'; 'g'], 4);
   (set ['c'; 'g'], 4); (set ['e'; 'g'], 4); (set ['g'], 4)]

*)
