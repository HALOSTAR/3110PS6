open Definitions
open Constants
open Util
open State
(**)



type game = state ref * int ref * Mutex.t (*No, This needs to be changed based
 on your implementation*)

let initGame () : game =
  let st = ref (empty ()) in
  let s = ref 0 in
  let m = Mutex.create () in
  Netgraphics.send_update(InitGraphics);
  Netgraphics.send_update(InitFood(cFOOD_TILES));
  Netgraphics.send_update(InitWood(cWOOD_TILES));
  (st, s, m)

(* game -> unit. Places intial units and buildings using pixels *)
let initUnitsAndBuildings ((st, s, m):game) : unit =
  let red_town_hall_loc = position_of_tile ((Random.int (cNUM_X_TILES/2-1))+1, (
    Random.int (cNUM_Y_TILES-1)+1)) in
  let blue_town_hall_loc = position_of_tile ((Random.int (cNUM_X_TILES/2-1))+
    (cNUM_X_TILES/2+1), (Random.int (cNUM_Y_TILES-1)+1)) in
  let get_adj_tile pos = 
    let (r, c) = tile_of_pos(pos) in
      if is_valid_tile (r+1, c) then (r+1, c)
      else if is_valid_tile (r-2, c) then (r-2, c)
      else if is_valid_tile (r, c-1) then (r, c-1)
      else (r, c+2) in
  let red_adj_tile = get_adj_tile red_town_hall_loc in
  let blue_adj_tile = get_adj_tile blue_town_hall_loc in 
  let rec init_units num t c = if num = 0 then [] else 
    let (a1,a2,a3,a4) = (next_available_id(),
    Villager, cVILLAGER_HEALTH, t) in
    Netgraphics.send_update (AddUnit(a1,a2,position_of_tile(a4),a3,c));
    (a1,a2,a3,position_of_tile(a4))::(init_units (num-1) t c) in
  let init_building l c = 
    let (a1,a2,a3,a4) = (next_available_id(), TownCenter, cTOWNCENTER_HEALTH,
    tile_of_pos(l)) in
    Netgraphics.send_update (AddBuilding(a1, a2, a4, a3, c)); [(a1,a2,a3,a4)] in
  let (a,b,c,(d,udr,bdr,e,f,g,h),(i,udb,bdb,j,k,n,o)) = !st in
  st := (a,b,c,(d,(init_units cSTARTING_VILLAGER_COUNT red_adj_tile Red),
    (init_building red_town_hall_loc Red),e,f,g,h),(i,(init_units 
    cSTARTING_VILLAGER_COUNT blue_adj_tile Blue), (init_building 
    blue_town_hall_loc Blue),j,k,n,o))

(* graphics updates already done *) 
let startGame g : unit = () 

let handleAction g act c : command = 
  let (st, s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
		| QueueCollect unit_id -> failwith "not implemented"
		| QueueMove(unit_id,pos) -> failwith "not implemented"
    | Talk str -> Netgraphics.add_update(DisplayString(c, str)); Success
		| QueueAttack (unit_id, attackable_object) -> failwith "not implemented"
		| QueueBuild (unit_id, building_type) -> failwith "not implemented"
		| QueueSpawn (building_id, unit_type) -> failwith "not implemented"
		| ClearAttack id -> failwith "not implemented" 
		| ClearMove id -> failwith "not implemented"
		| Upgrade upgrade_type -> failwith "not implemented"
		in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (st, s, m) = g in
  Mutex.lock m;
  let data =
    match status with
			| TeamStatus c -> failwith "not implemented"
			| UnitStatus id -> failwith "not implemented"
			| BuildingStatus id -> failwith "not implemented"
			| GameStatus -> failwith "not implemented"
			| ResourceStatus -> failwith "not implemented"
    in
  Mutex.unlock m;
  Data data

let check_for_game_over s curr_time : game_result option =
	failwith "not implemented"

let handleTime g new_time : game_result option = 
  let (st, s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  (match res with
   | Some c -> ()
   | None -> 
       failwith "not implemented");
  Mutex.unlock m;
  res
