open Definitions
open Constants
open Util
open State

type game = state ref * float ref * Mutex.t (*No, This needs to be changed based
 on your implementation*)

(*let initGame () : game =
  let st = ref (empty ()) in
  let s = ref 0 in
  let m = Mutex.create () in
  Netgraphics.send_update(InitGraphics);
  Netgraphics.send_update(InitFood(cFOOD_TILES));
  Netgraphics.send_update(InitWood(cWOOD_TILES));
  (st, s, m)*)

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
    Netgraphics.send_update (AddUnit(a1,a2,position_of_tile(4,4),a3,c));
    (a1,a2,a3,position_of_tile(a4))::(init_units (num-1) t c) in
  let init_building l c = 
    let (a1,a2,a3,a4) = (next_available_id(), TownCenter, cTOWNCENTER_HEALTH,
    tile_of_pos(l)) in
    Netgraphics.send_update (AddBuilding(a1, a2,(2,2),a3, c));[(a1,a2,a3,a4)] in
  let (a,b,c,(d,udr,bdr,e,f,g,h),(i,udb,bdb,j,k,n,o)) = !st in
  (*st := (a,b,c,(d,(init_units cSTARTING_VILLAGER_COUNT red_adj_tile Red),
    (init_building red_town_hall_loc Red),e,f,g,h),(i,(init_units 
    cSTARTING_VILLAGER_COUNT blue_adj_tile Blue), (init_building 
    blue_town_hall_loc Blue),j,k,n,o))*)
 st := (a,b,c,(d,(init_units cSTARTING_VILLAGER_COUNT red_adj_tile Red),
    (init_building red_town_hall_loc Red),e,f,g,h),(i,(init_units 
    cSTARTING_VILLAGER_COUNT blue_adj_tile Blue), (init_building 
    blue_town_hall_loc Blue),j,k,n,o))

(* graphics updates already done *) 
let startGame g : unit = 
  let (st, s, m) = g in
  s := Unix.gettimeofday()

let handleAction g act c : command = 
  let (st, s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
     (* | QueueCollect unit_id ->
          let pos = checkid_checkunit !st unit_id c in
          let (boolean, state) = update_score_resource !st pos c in
          if (pos <> (-1., -1.) && boolean)
          then ((st := state); 
Success) else 
(Netgraphics.add_update(DisplayString(c, "failed")); Failed)*)
      | QueueMove(unit_id,pos) -> if (check_unit_id !st c unit_id = false) || 
((is_valid_pos pos) = false) then Failed 
        else (st := (queue_moves !st c unit_id 
(get_moves (!st) (unit_id) (c) (pos))); Success) 
            
    | Talk str -> Netgraphics.add_update(DisplayString(c, str)); Success
    | QueueAttack (unit_id, attackable_object) ->
        let target_id =
          match attackable_object with
            Building b -> b
            | Unit u -> u in
        if ((get_unit_type !st c unit_id) = Villager) || 
           ((check_unit_id !st c unit_id )= false) || 
           (check_unit_id !st c target_id) ||
           ((get_unit_health !st c unit_id) = 0) 
        then Failed
        else (st := (queue_attack !st c unit_id attackable_object); Success)

    | QueueBuild (unit_id, building_type) -> 
        if (building_type <> Barracks) ||
           ((check_unit_id !st c unit_id) = false) ||
           ((get_unit_type !st c unit_id) <> Villager) ||
           ((check_tiles_valid_empty !st c unit_id) = false) ||
           ((check_enough_resources !st c building_type) = false) then Failed
        else (st := (start_build !st c unit_id); Success)

    | QueueSpawn (building_id, unit_type) -> 
        if ((check_unit_id !st c building_id) = false) ||
           ((check_enough_resources !st c unit_type) = false) ||
           ((unit_matches_upgrade !st c unit_type) = false) ||
           ((unit_matches_age !st c unit_type) = false) then Failed
        else (spawn_unit !st c building_id unit_type; Success)

    | ClearAttack id ->
        if ((check_unit_id !st c id) = false) then Failed
        else (clear_attack !st c id; Success)

    | ClearMove id ->
        if ((check_unit_id !st c id) = false) then Failed
        else (clear_move !st c id; Success)

    | Upgrade upgrade_type ->
        if upgrade_types !st c upgrade_type then Success else Failed
  in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (st, s, m) = g in
  Mutex.lock m;
  let data =
    match status with
      | TeamStatus c -> TeamData (get_team_status !st c)
      | UnitStatus id -> UnitData (get_unit_status !st id)
      | BuildingStatus id -> BuildingData (get_building_status !st id)
      | GameStatus -> GameData ((get_team_status !st Red), 
          (get_team_status !st Blue), !s)
      | ResourceStatus -> ResourceData (get_resource_status !st)
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
       handle_attacks !st


);
  Mutex.unlock m;
  res
