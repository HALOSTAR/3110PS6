open Constants
open Definitions
open Util
open Netgraphics

(* unit_data record *)
type udrec = {mutable udrec_uid:unit_id; mutable udrec_ut:unit_type; 
mutable udrec_h:health; mutable udrec_pos:position}

(* building_data record *)
type bdrec = {mutable bdrec_bi:building_id; mutable bdrec_bt:building_type; 
mutable bdrec_h:health; mutable bdrec_t:tile}

(* upgrades record *)
type uprec = {mutable pikeman:bool; mutable archer:bool; mutable knight:bool}

(* team_data record *)
type tdrec = {mutable s:score; mutable udl:udrec list; mutable bdl:bdrec list; 
mutable a:age; mutable fc:food_count; mutable wc:wood_count; mutable up:uprec}

(* resource_data record *)
type rdrec = {rdrec_t:tile; mutable rt:resource_type; mutable i:int}

(* cooldown time record *)
type cdrec = {mutable cdrec_uid:unit_id; mutable ctime:float;
mutable starttime:float; mutable building:bool}

(* Move Queue - not complete *)
type mqueue = {mutable mqueue_uid:unit_id; mutable moves:position Queue.t}

(* Attack Queue - not complete *) 
type aqueue = {mutable aqueue_uid:unit_id; mutable attacks:attackable_object
Queue.t}


(* state record
 * red resource list   blue resource list
 * red team data       blue team data
 * red cooldown time   blue cooldown time
 * red move queue      blue move queue *)
type state = {mutable resource_list:rdrec list; 
mutable red_team_data:tdrec; mutable blue_team_data:tdrec;
mutable red_ctime:cdrec list; mutable blue_ctime:cdrec list;
mutable red_move:mqueue list; mutable blue_move:mqueue list; 
mutable red_attack:aqueue list; mutable blue_attack:aqueue list}

let empty_upgrades () : uprec =
   {pikeman=false; archer=false; knight=false}

let empty_team_data () : tdrec =
   {s=0; udl= []; bdl= []; a=DarkAge; fc=0; wc=0; up=empty_upgrades() }

let init_resources : rdrec list =
  let init_food =
    List.fold_left (fun acc hd -> {rdrec_t=hd; rt=Food; i=cINITIAL_FOOD}::acc)
    [] cFOOD_TILES in
  let init_wood =
    List.fold_left (fun acc hd -> {rdrec_t=hd; rt=Wood; i=cINITIAL_WOOD}::acc)
    init_food cWOOD_TILES in
  init_wood

let empty () : state =
  { resource_list=init_resources;
   red_team_data=empty_team_data(); 
   blue_team_data=empty_team_data();
   red_ctime=[]; blue_ctime=[]; red_move=[]; blue_move=[]; red_attack=[];
   blue_attack=[] }

let init_unit_state (st:state) (id:unit_id) (pos:position) (c:color) : unit =
  let new_unit = {udrec_uid=id; udrec_ut=Villager; udrec_h=cVILLAGER_HEALTH;
    udrec_pos=pos} in
  let new_ctime ={cdrec_uid=id; ctime=(-1.); starttime=(-1.); building=false} in
  let new_move = {mqueue_uid=id; moves=(Queue.create())} in
  if c = Red then
  (st.red_team_data.udl <- new_unit::(st.red_team_data.udl);
  st.red_ctime <- new_ctime::(st.red_ctime);
  st.red_move <- new_move::(st.red_move))
  else
 (st.blue_team_data.udl <- new_unit::(st.blue_team_data.udl);
  st.blue_ctime <- new_ctime::(st.blue_ctime);
  st.blue_move <- new_move::(st.blue_move))

let init_unit_gui (id:unit_id) (pos:position) (c:color) : unit =
  Netgraphics.add_update (AddUnit (id, Villager, pos, cVILLAGER_HEALTH, c))

let init_building_state (st:state) (id:building_id) (t:tile) (c:color) : unit =
  let new_building = {bdrec_bi=id; bdrec_bt=TownCenter; bdrec_h=
    cTOWNCENTER_HEALTH; bdrec_t=t} in
  if c = Red then
  (st.red_team_data.bdl <- new_building::(st.red_team_data.bdl))
  else
  (st.blue_team_data.bdl <- new_building::(st.blue_team_data.bdl))

let init_building_gui (id:building_id) (t:tile) (c:color) : unit =
  Netgraphics.add_update (AddBuilding (id, TownCenter, t, cTOWNCENTER_HEALTH, 
  c))

(* For handleAction
 * Given: a state, color c, and unit_id
 * Returns: whether the unit_id belongs to c *)
let check_unit_id (st:state) (c:color) (id:unit_id) : bool =
  let r_b_teamdata = if (c=Red) then st.red_team_data else st.blue_team_data in
  let check_ids lst = List.fold_left (fun acc hd -> if hd.udrec_uid = id then
true else acc) false lst in
  check_ids (r_b_teamdata.udl)

(* For QueueSpawn:
 * Takes: state, color of team spawning unit, building_id spawning unit
 * Returns: whether the building_id belongs to c *)
let check_building_id (st:state) (c:color) (id:building_id) : bool =
  let r_b_teamdata = if (c=Red) then st.red_team_data else st.blue_team_data in
  let check_ids lst = List.fold_left (fun acc hd -> if hd.bdrec_bi = id then
true else acc) false lst in
  check_ids (r_b_teamdata.bdl)

(* Returns the unit type of a unit given its id *)
let get_unit_type (st:state) (c:color) (id:unit_id) : unit_type =
  let unit_data_list = if c = Red then st.red_team_data.udl else
st.blue_team_data.udl in
  List.fold_left (fun acc hd -> if hd.udrec_uid = id then hd.udrec_ut else acc)
  Villager unit_data_list 

(* Takes: state, color, and unit id
 * Returns: The position of the unit *)
let get_unit_pos (st:state) (c:color) (id:unit_id) : position =
  let unit_data_list = if c = Red then st.red_team_data.udl else
st.blue_team_data.udl in
    let get_pos lst = List.fold_left (fun acc hd -> if hd.udrec_uid = id then 
hd.udrec_pos else acc) (-1., -1.) lst in
  get_pos (unit_data_list)

let check_on_resource (st:state) (c:color) (id:unit_id) : bool =
  let curr_pos = tile_of_pos (get_unit_pos st c id) in
  List.fold_left (fun acc hd -> if hd.rdrec_t = curr_pos then true else acc)
    false st.resource_list

(* Changes the score of team c, updates the resource count of team c and 
 * total resources available *)
let collect_resource (st:state) (c:color) (id:unit_id) : unit =
  let curr_pos =  tile_of_pos (get_unit_pos st c id) in
  let teamdata = if c = Red then st.red_team_data else st.blue_team_data in
  let rtype = List.fold_left (fun acc hd -> if hd.rdrec_t = curr_pos then
    hd.rt else acc) Food st.resource_list in
  let to_collect =
    if teamdata.a = DarkAge then cRESOURCE_COLLECTED else
      cADVANCED_RESOURCE_COLLECTED in
  let amount_remaining = 
    List.fold_left (fun acc hd -> if hd.rdrec_t = curr_pos then hd.i else acc)
      0 st.resource_list in
  let collected = min to_collect amount_remaining in        
  let update_count =
    List.fold_left (fun acc hd -> if hd.rdrec_t = curr_pos then
      (hd.i <- (if ((hd.i - collected) <= 0) then 0 
      else (hd.i - collected)); hd.i) else acc) (-1) st.resource_list in
  let update_ctime_list = 
    let ctime_list = if c = Red then st.red_ctime else st.blue_ctime in
    List.fold_left (fun acc hd -> if hd.cdrec_uid = id then (hd.ctime <-
      cVILLAGER_COOLDOWN; hd.starttime <- Unix.gettimeofday()) else acc) ()
      ctime_list in
  let update_state = 
    teamdata.s <- ((teamdata.s) + collected);
    update_ctime_list;
    if rtype = Food then (teamdata.fc <- (teamdata.fc + collected))
    else (teamdata.wc <- (teamdata.wc + collected)) in
  let update_gui = Netgraphics.add_update (DoCollect (id, c, rtype, collected));
    Netgraphics.add_update (UpdateScore (c, teamdata.s));
    Netgraphics.add_update (UpdateResource (curr_pos, update_count)) in
  update_state; update_gui


(* Returns the tile of the building given its id *)
let get_building_tile (st:state) (c:color) (id:building_id) : tile =
  let building_data_list = if c = Red then st.red_team_data.bdl else
st.blue_team_data.bdl in
    let get_pos lst = List.fold_left (fun acc hd -> if hd.bdrec_bi = id then 
hd.bdrec_t else acc) (-1, -1) lst in
  get_pos (building_data_list)

(* For QueueMove:
 * Takes a state, unit_id, and final position
 * Returns a list of intermediate positions *)
let get_moves (st:state) (id:unit_id) (c:color) (pos:position) : 
position Queue.t =
  let current_position = get_unit_pos st c id in
  let normalized_vector start dest =
    let (x1,y1) = start in
    let (x2,y2) = dest in
    let dist = distance (x1, y1) (x2, y2) in
  ((x2-.x1)/.dist,(y2-.y1)/.dist) in
  let unit_speed = get_unit_type_speed (get_unit_type st c id) in
  let moves_queue = Queue.create () in
  let rec enqueue_moves curr =
    if (distance (curr) (pos)) <= unit_speed then(Queue.add (pos) (moves_queue))
    else 
      let (inter_posx, inter_posy) =
        let (x1, y1) = curr in
        let (x2, y2) = normalized_vector curr pos in
(x1 +. (x2 *. unit_speed), y1 +. (y2 *. unit_speed)) in
      Queue.add (inter_posx, inter_posy) (moves_queue); enqueue_moves
(inter_posx, inter_posy)  in
  if (current_position = pos) then moves_queue else 
(enqueue_moves (current_position); moves_queue)

(* For QueueMove:
 * Takes: state, color, unit id, queue of moves to add
 * Returns: a new state with the moves added to the right unit *)
let queue_moves (st:state) (c:color) (id:unit_id) (moves:position Queue.t) :
unit =
  let team_move_list = if c = Red then st.red_move else st.blue_move in
  let team_moves_queue lst = List.fold_left (fun acc hd -> if hd.mqueue_uid =id 
                                            then hd.moves else acc) 
(Queue.create()) lst in
  Queue.transfer (moves) (team_moves_queue team_move_list)

(* Gets the unit's health 
 * Takes: state, color, unit id 
 * Returns: health *)
let get_unit_health (st:state) (c:color) (id:unit_id) : health =
  let unit_list = if c = Red then st.red_team_data.udl else 
st.blue_team_data.udl in
  let find_health lst = List.fold_left (fun acc hd -> if hd.udrec_uid = id
                                        then hd.udrec_h else acc) (-1) lst in
  find_health (unit_list)

let get_building_health (st:state) (c:color) (id:building_id) : health =
  let building_list = if c = Red then st.red_team_data.bdl else 
st.blue_team_data.bdl in
  let find_health lst = List.fold_left (fun acc hd -> if hd.bdrec_bi = id
                                        then hd.bdrec_h else acc) (-1) lst in
  find_health (building_list)

(* For QueueAttack:
 * Takes: state, color of attacker, unit_id, target 
 * Returns: a new state *)
let queue_attack (st:state) (c:color) (id:unit_id) (target:attackable_object) :
unit =
  let attack_list = if c = Red then st.red_attack else st.blue_attack in
  let find_queue lst = List.fold_left (fun acc hd -> if hd.aqueue_uid = id then
hd.attacks else acc) (Queue.create()) lst in
  let target_as_queue =
    let q = Queue.create() in
  Queue.add target q; q in
  Queue.transfer (target_as_queue) (find_queue attack_list)

(* Checks if a group of 2x2 tiles are valid and empty
 * Takes: state, color, unit_id
 * Returns: boolean *)
let check_tiles_valid_empty (st:state) (c:color) (id:unit_id) : bool =
  let current_tile = tile_of_pos (get_unit_pos st c id) in
  let check_valid (r, c) =
       ((is_valid_tile (r,c)) && (is_valid_tile (r+1,c)) &&
       (is_valid_tile (r,c+1)) && (is_valid_tile (r+1,c+1))) in
  let check_empty (r1,c1) (r2,c2) (r3,c3) (r4,c4) =
    let check_resources lst = List.fold_left (fun acc hd -> 
      if (hd.i > 0) &&
         ((hd.rdrec_t = (r1,c1)) || (hd.rdrec_t = (r2,c2)) ||
         (hd.rdrec_t = (r3,c3)) || (hd.rdrec_t = (r4,c4))) then false else acc)
      true lst in
    let check_buildings lst = List.fold_left (fun acc hd ->
      if (hd.bdrec_h > 0) &&
         ((hd.bdrec_t = (r1,c1)) || (hd.bdrec_t = (r2,c2)) ||
         (hd.bdrec_t = (r3,c3)) || (hd.bdrec_t = (r4,c4))) then false else acc)
      true lst in
    let check_already_building lst = List.fold_left (fun acc hd ->
      if (hd.building) &&
         ((tile_of_pos (get_unit_pos st c (hd.cdrec_uid))) = current_tile) then
      false else acc) true lst in
    (check_resources st.resource_list) && (check_buildings st.red_team_data.bdl)
     && (check_buildings st.blue_team_data.bdl) && (check_already_building
     st.red_ctime) && (check_already_building st.blue_ctime) in
  let (tilex, tiley) = current_tile in
  (check_valid current_tile) && (check_empty (tilex,tiley) (tilex+1,tiley)
  (tilex,tiley+1) (tilex+1,tiley+1))

let valid_empty (st:state) (c:color) (current_tile:tile) : bool =
  let check_valid (r, c) =
       ((is_valid_tile (r,c)) && (is_valid_tile (r+1,c)) &&
       (is_valid_tile (r,c+1)) && (is_valid_tile (r+1,c+1))) in
  let check_empty (r1,c1) (r2,c2) (r3,c3) (r4,c4) =
    let check_resources lst = List.fold_left (fun acc hd -> 
      if (hd.i > 0) &&
         ((hd.rdrec_t = (r1,c1)) || (hd.rdrec_t = (r2,c2)) ||
         (hd.rdrec_t = (r3,c3)) || (hd.rdrec_t = (r4,c4))) then false else acc)
      true lst in
    let check_buildings lst = List.fold_left (fun acc hd ->
      if (hd.bdrec_h > 0) &&
         ((hd.bdrec_t = (r1,c1)) || (hd.bdrec_t = (r2,c2)) ||
         (hd.bdrec_t = (r3,c3)) || (hd.bdrec_t = (r4,c4))) then false else acc)
      true lst in
    let check_already_building lst = List.fold_left (fun acc hd ->
      if (hd.building) &&
         ((tile_of_pos (get_unit_pos st c (hd.cdrec_uid))) = current_tile) then
      false else acc) true lst in
    (check_resources st.resource_list) && (check_buildings st.red_team_data.bdl)
     && (check_buildings st.blue_team_data.bdl) && (check_already_building
     st.red_ctime) && (check_already_building st.blue_ctime) in
  let (tilex, tiley) = current_tile in
  (check_valid current_tile) && (check_empty (tilex,tiley) (tilex+1,tiley)
  (tilex,tiley+1) (tilex+1,tiley+1))

let rec choose_tile_Red (st:state) (t:tile) : tile =
  if not (valid_empty st Red t) || not (valid_empty st Blue t) then
    let y = Random.int (cNUM_X_TILES/2) - 1 in
    let x = Random.int (cNUM_Y_TILES) - 1 in
  choose_tile_Red st (x, y)
  else t

let rec choose_tile_Blue (st:state) (t:tile) : tile =
  if not (valid_empty st Red t) || not (valid_empty st Blue t) then
    let y = Random.int (cNUM_X_TILES/2) + (cNUM_X_TILES/2) - 1 in
    let x = Random.int (cNUM_Y_TILES) - 1 in
  choose_tile_Blue st (x, y)
  else t

(* For QueueBuild - checks if there are enough resources to build 
 * Takes: state, color of team, building_type to be built
 * Returns: bool *)
let check_enough_resources_building (st:state) (c:color) (btype:building_type) :
bool =
  let (food_count, wood_count) = if c = Red then (st.red_team_data.fc,
st.red_team_data.wc) else (st.blue_team_data.fc, st.blue_team_data.wc) in
  let (food_cost, wood_cost) =
  match btype with
    TownCenter -> cTOWNCENTER_COST
    | Barracks -> cBARRACKS_COST in
  (food_cost <= food_count) && (wood_cost <= wood_count)

(* Returns the (food, wood) cost of the unit *)
let get_unit_cost (utype:unit_type) : int*int =
  match utype with
    Villager -> cSPAWN_VILLAGER_COST
    | Pikeman -> cSPAWN_PIKEMAN_COST
    | Archer -> cSPAWN_ARCHER_COST
    | Knight -> cSPAWN_KNIGHT_COST 
    | EliteKnight -> cSPAWN_KNIGHT_COST
    | EliteArcher -> cSPAWN_ARCHER_COST
    | ElitePikeman -> cSPAWN_PIKEMAN_COST

(* For QueueSpawn - checks if there are enough resources to spawn
 * Takes: state, color of team, unit_type to be spawned
 * Returns: bool *)
let check_enough_resources_unit (st:state) (c:color) (utype:unit_type) : bool =
  let (food_count, wood_count) = if c = Red then (st.red_team_data.fc,
st.red_team_data.wc) else (st.blue_team_data.fc, st.blue_team_data.wc) in
  let (food_cost, wood_cost) = get_unit_cost utype in
  (food_cost <= food_count) && (wood_cost <= wood_count)

(* For QueueBuild - subtract cost, add the unit_id and cooldown time to list 
 * Takes: state, color of builder, unit_id of builder
 * Returns: a new state *)   
let start_build (st:state) (c:color) (id:unit_id) : unit =
  let (food_cost,wood_cost) = cBARRACKS_COST in
  let team_build_list = if c = Red then st.red_ctime else 
st.blue_ctime in
  let team_data_list = if c = Red then st.red_team_data else
st.blue_team_data in
  let subtract_resources lst = lst.fc <- (lst.fc - food_cost);
lst.wc <- (lst.wc - wood_cost) in
  let update_builder lst = List.fold_left (fun acc hd -> if
    hd.cdrec_uid = id then (hd.starttime <- (Unix.gettimeofday()); hd.ctime <-
    cVILLAGER_COOLDOWN; hd.building <- true; Netgraphics.add_update (DoCollect
    (hd.cdrec_uid, c, Food, ((-1)*(food_cost)))); Netgraphics.add_update
    (DoCollect (hd.cdrec_uid, c, Wood, ((-1)*(wood_cost))))) else
     acc) () lst in
  subtract_resources (team_data_list); update_builder (team_build_list)

(* For QueueSpawn - checks if the unit_type to be spawned is correct level 
 * Takes: state, color of spawning team, unit_type to be spawned
 * Returns: bool *)
let unit_matches_upgrade (st:state) (c:color) (utype:unit_type) : bool =
  let unit_upgrades = if c = Red then st.red_team_data.up else
st.blue_team_data.up in
  match utype with
    ElitePikeman -> unit_upgrades.pikeman
    | EliteArcher -> unit_upgrades.archer
    | EliteKnight -> unit_upgrades.knight
    | Pikeman -> (unit_upgrades.pikeman = false)
    | Archer -> (unit_upgrades.archer = false)
    | Knight -> (unit_upgrades.knight = false)
    | _ -> true
            
(* For QueueSpawn - checks if the unit type is in the right age 
 * Takes: state, color of team, unit_type to be spawned
 * Returns: whether the unit is in the right age - bool *)
let unit_matches_age (st:state) (c:color) (utype:unit_type) : bool =
  let team_age = if c = Red then st.red_team_data.a else
st.blue_team_data.a in
  match utype with
    Archer | Knight | ElitePikeman | EliteArcher | EliteKnight ->
      team_age = ImperialAge
    | _ -> true

(* Returns a valid adjacent tile to the 2x2 building tile given 
 * Used for placing units next to a building *)
let get_adj_tile (t:tile) : tile =
  let (r, c) = t in
  if (is_valid_tile (r-1, c)) then (r-1, c)
  else if (is_valid_tile (r+2, c)) then (r+2, c)
  else if (is_valid_tile (r, c-1)) then (r, c-1)
  else if (is_valid_tile (r, c+2)) then (r, c+2)
  else (r, c) 

let unit_matches_building (st:state) (c:color) (bi:building_id)
(utype:unit_type) : bool =
  let bdata = if c = Red then st.red_team_data.bdl else st.blue_team_data.bdl in
  let btype = List.fold_left (fun acc hd ->
    if hd.bdrec_bi = bi then hd.bdrec_bt else acc) Barracks bdata in
  (btype = TownCenter && utype = Villager) ||
  (btype = Barracks && (not (utype = Villager)))

(* For QueueSpawn - spawns the unit for the given team, updates state
 * Takes: state, color, the building for spawning, the type to spawn
 * Returns: an updated state *)
let spawn_unit (st:state) (c:color) (id:building_id) (utype:unit_type) :
unit =
  let (food_cost, wood_cost) = get_unit_cost utype in
  let subtract_costs = 
    let team_data = if c = Red then st.red_team_data else st.blue_team_data in
    let (food_count, wood_count) = (team_data.fc, team_data.wc) in
  (team_data.fc <- (food_count - food_cost)); (team_data.wc <- (wood_count -
  wood_cost)) in
  let pos_of_spawned = 
    let tile_of_building = get_building_tile st c id in
    position_of_tile (get_adj_tile tile_of_building) in
  let id_of_spawned = next_available_id() in
  let health_of_spawned = get_unit_type_health utype in
  let add_to_state =
    let new_unit_data = {udrec_uid=id_of_spawned; udrec_ut=utype; udrec_h=
      health_of_spawned; udrec_pos=pos_of_spawned} in
    let new_ctime = {cdrec_uid=id_of_spawned; ctime=(-1.); starttime=(-1.);
      building=false} in
    let new_moves = {mqueue_uid=id_of_spawned; moves=(Queue.create())} in
    let new_attacks = {aqueue_uid=id_of_spawned; attacks=(Queue.create())} in
    if c = Red then
     (st.red_team_data.udl <- (new_unit_data::st.red_team_data.udl);
      st.red_ctime <- (new_ctime::st.red_ctime);
      st.red_move <- (new_moves::st.red_move);
      st.red_attack <- (new_attacks::st.red_attack))
    else
      (st.blue_team_data.udl <- (new_unit_data::st.blue_team_data.udl);
      st.blue_ctime <- (new_ctime::st.blue_ctime);
      st.blue_move <- (new_moves::st.blue_move);
      st.blue_attack <- (new_attacks::st.blue_attack)) in
  let add_to_gui =
    Netgraphics.add_update (AddUnit (id_of_spawned, utype, pos_of_spawned,
    health_of_spawned, c)); Netgraphics.add_update (DoCollect (id_of_spawned,
    c, Food, ((-1)*(food_cost)))); Netgraphics.add_update (DoCollect (
    id_of_spawned, c, Wood, ((-1)*(wood_cost)))) in
  subtract_costs; add_to_state; add_to_gui

(* For ClearAttack - finds the attack queue of a given unit id 
 * Takes: state, color id belongs to, unit id you want to clear
 * Returns: unit *)
let clear_attack (st:state) (c:color) (id:unit_id) : unit =
  let attack_queue = if c = Red then st.red_attack else st.blue_attack in
  let clear_queue lst = List.fold_left (fun acc hd -> if hd.aqueue_uid = id then
    (Queue.clear hd.attacks) else acc) () lst in
  clear_queue attack_queue

(* For ClearMoves - finds the moves queue of given unit id
 * Takes: state, color id belongs to, unit id you want to clear
 * Returns: unit *)
let clear_move (st:state) (c:color) (id:unit_id) : unit =
  let moves_queue = if c = Red then st.red_move else st.blue_move in
  let clear_queue lst = List.fold_left (fun acc hd -> if hd.mqueue_uid = id then
    (Queue.clear hd.moves) else acc) () lst in
  clear_queue moves_queue

(* Upgrades the age for color c - Called by upgrade_types *)
let upgrade_age (st:state) (c:color) : bool =
  let team_data = if c = Red then st.red_team_data else st.blue_team_data in
  let (food_cost, wood_cost) = cUPGRADE_AGE_COST in
  let (food_count, wood_count) = (team_data.fc, team_data.wc) in
  let upgrade_state = 
    team_data.fc <- (food_count-food_cost);team_data.wc <- 
    (wood_count-wood_cost); (team_data.a <- ImperialAge) in
  let upgrade_gui = Netgraphics.add_update (UpgradeAge (c));
    Netgraphics.add_update (DoCollect (0, c, Food, ((-1)*food_cost)));
    Netgraphics.add_update (DoCollect (0, c, Wood, ((-1)*wood_cost))) in
  if (team_data.a = DarkAge) then (upgrade_state; upgrade_gui; true)
  else false

(* For AgeUpgrade - checks if color is already in ImperialAge *)
let age_already_upgraded (st:state) (c:color) : bool =
  let agedata = if c = Red then st.red_team_data.a else st.blue_team_data.a in
  agedata = ImperialAge

(* For UnitUpgrade - checks if the unit is already upgraded *)
let unit_already_upgraded (st:state) (c:color) (utype:unit_type) : bool =
  let teamdata = if c = Red then st.red_team_data else st.blue_team_data in
  match utype with
    Pikeman | ElitePikeman -> teamdata.up.pikeman
    | Archer | EliteArcher -> teamdata.up.archer
    | Knight | EliteKnight -> teamdata.up.knight
    | _ -> false

(* Upgrades a unit type for the state and gui 
 * Called by upgrade_types *)
let upgrade_unit (st:state) (c:color) (utype:unit_type) : bool =
  let team_data = if c = Red then st.red_team_data else st.blue_team_data in
  let (inf_version, elite_version) =
    match utype with
      Pikeman | ElitePikeman -> (Pikeman, ElitePikeman)
      | Archer | EliteArcher -> (Archer, EliteArcher)
      | Knight | EliteKnight -> (Knight, EliteKnight)
      | _ -> failwith "Already checked Villager in upgrade_types" in
  let upgrade_record =
     match elite_version with
      ElitePikeman -> team_data.up.pikeman <- true
      | EliteArcher -> team_data.up.archer <- true
      | EliteKnight -> team_data.up.knight <- true 
      | _ -> () in
  let check_age = unit_matches_age st c utype in
  let (food_cost, wood_cost) =
    match elite_version with
      ElitePikeman -> cUPGRADE_PIKEMAN_COST
      | EliteArcher -> cUPGRADE_ARCHER_COST
      | EliteKnight -> cUPGRADE_KNIGHT_COST
      | _ -> (-1, -1) in
  let upgrade_state =
    let (food_count, wood_count) = (team_data.fc, team_data.wc) in
    let traverse_units lst = List.fold_left (fun acc hd ->
      if hd.udrec_ut = inf_version then hd.udrec_ut <- elite_version
      else acc) () lst in
    traverse_units (team_data.udl); team_data.fc <-(food_count-food_cost); 
    team_data.wc <-(wood_count-wood_cost) in
  let upgrade_gui = Netgraphics.add_update (UpgradeUnit (inf_version, c));
    Netgraphics.add_update (DoCollect (0, c, Food, ((-1)*(food_cost))));
    Netgraphics.add_update (DoCollect (0, c, Wood, ((-1)*(wood_cost)))) in
  if (check_age) then (upgrade_state;
    upgrade_gui; upgrade_record; true) else false

(* For Upgrade - checks if the team has enough resources, if the upgrade is 
 * appropriate for the age, and whether the upgrade has already been done *)
let upgrade_types (st:state) (c:color) (uptype:upgrade_type) : bool =
  let check_cost =
    let (food_cost, wood_cost) =
      match uptype with
        AgeUpgrade -> cUPGRADE_AGE_COST
        | UnitUpgrade u ->
            match u with
              Knight | EliteKnight -> cUPGRADE_KNIGHT_COST
              | Archer | EliteArcher -> cUPGRADE_ARCHER_COST
              | Pikeman | ElitePikeman -> cUPGRADE_PIKEMAN_COST 
              | _ -> (-1, -1) in
    let (food_count, wood_count) = if c = Red then (st.red_team_data.fc,
      st.red_team_data.wc) else (st.blue_team_data.fc, st.blue_team_data.wc) in
    ((food_cost, wood_cost) <> (-1, -1)) && (food_cost <= food_count) && 
    (wood_cost <= wood_count) in
  if check_cost then
    match uptype with
      AgeUpgrade -> (upgrade_age st c)
      | UnitUpgrade utype -> (upgrade_unit st c utype)
  else false

let unit_data_to_tuple (record_form:udrec) : unit_data =
  (record_form.udrec_uid, record_form.udrec_ut, record_form.udrec_h, 
   record_form.udrec_pos)

let building_data_to_tuple (record_form:bdrec) : building_data =
  (record_form.bdrec_bi, record_form.bdrec_bt, record_form.bdrec_h, 
   record_form.bdrec_t)

let upgrades_to_tuple (record_form:uprec) : upgrades =
  (record_form.pikeman, record_form.archer, record_form.knight)

let resource_data_to_tuple (record_form:rdrec) : resource_data =
  (record_form.rdrec_t, record_form.rt, record_form.i)

let unit_data_to_list (record_list:udrec list) : unit_data list =
  let helper = List.fold_left (fun acc hd -> (unit_data_to_tuple hd)::acc) [] in
  helper record_list
   
let building_data_to_list (record_list:bdrec list) : building_data list =
  let helper = List.fold_left (fun acc hd -> (building_data_to_tuple hd)::acc) 
    [] in
  helper record_list

let resource_data_to_list (record_list:rdrec list) : resource_data list =
  let helper = List.fold_left (fun acc hd -> (resource_data_to_tuple hd)::acc) 
    [] in
  helper record_list

let get_team_status (st:state) (c:color) : team_data =
  let data_record = if c = Red then st.red_team_data else st.blue_team_data in
  (data_record.s, unit_data_to_list (data_record.udl), 
   building_data_to_list (data_record.bdl), data_record.a,
  data_record.fc, data_record.wc, upgrades_to_tuple (data_record.up))

(* For UnitStatus - assumes unit_data exists *)
let get_unit_status (st:state) (id:unit_id) : unit_data =
  let traverse lst = List.fold_left (fun acc hd -> if hd.udrec_uid = id then
    hd else acc) {udrec_uid=(-1);udrec_ut=Villager;udrec_h=(-1);udrec_pos=
    (-1.,-1.)} lst in
  let traversed_red = traverse st.red_team_data.udl in
  let traversed_blue = traverse st.blue_team_data.udl in
  if (traversed_red) <> {udrec_uid=(-1);udrec_ut=Villager;udrec_h=(-1);
    udrec_pos=(-1.,-1.)} then unit_data_to_tuple (traversed_red) else
    unit_data_to_tuple (traversed_blue)

(* For BuildingStatus - assumes building_data exists *)
let get_building_status (st:state) (id:building_id) : building_data =
  let traverse lst = List.fold_left (fun acc hd -> if hd.bdrec_bi = id then
    hd else acc) {bdrec_bi=(-1);bdrec_bt=TownCenter;bdrec_h=(-1);bdrec_t=
    (-1,-1)} lst in
  let traversed_red = traverse st.red_team_data.bdl in
  let traversed_blue = traverse st.blue_team_data.bdl in
  if (traversed_red) <> {bdrec_bi=(-1);bdrec_bt=TownCenter;bdrec_h=(-1);
    bdrec_t=(-1,-1)} then building_data_to_tuple (traversed_red) else
    building_data_to_tuple (traversed_blue)

(* Called by ResourceStatus to return a resource data list *)
let get_resource_status (st:state) : resource_data list =
  resource_data_to_list st.resource_list

(* Takes the color of the target, unit_type and position of the attacker, and
 * the queue of attacks for the attacker.
 * Returns: Queue of attacks with the first being valid; unit
 * Validity: target is in range, not dead *)
let rec find_first_attack (st:state) (ct:color) (utype:unit_type) (pos:position)
(attacks:attackable_object Queue.t) : unit = if (Queue.length attacks = 0) then
() else
  let target_health =
    match Queue.peek attacks with
      Building b ->
        get_building_health st ct b
      | Unit u -> get_unit_health st ct u in
  let check_range =
    let (targetx, targety) = 
      match Queue.peek attacks with
        Building b ->
          position_of_tile (get_building_tile st ct b)
        | Unit u -> get_unit_pos st ct u in
    let range = length_of_range (get_unit_type_range utype) in
    let (r, c) = tile_of_pos (targetx, targety) in
    match Queue.peek attacks with
      Building b ->
    ((range >= (distance pos (targetx, targety))) ||
    (range >= (distance pos (position_of_tile (r+1,c)))) ||
    (range >= (distance pos (position_of_tile (r,c+1)))) ||
    (range >= (distance pos (position_of_tile (r+1,c+1))))) 
      | Unit u ->
         range >= (distance pos (targetx, targety)) in
  if (target_health > 0 && check_range) then ()
  else if (Queue.length attacks) = 1 then ignore(Queue.pop attacks)
  else (ignore(Queue.pop attacks); find_first_attack st ct utype pos attacks)

(* For handleTime - finds (if any) valid attacks for each unit and removes
 * them from the attack queue. Also subtracts health from targets. Finally,
 * updates the GUI *)  
let handle_attacks (st:state) (curr_time:float) : unit =
  let check_not_coolingdown ca id =
    let team_cooldowns = if ca = Red then st.red_ctime else st.blue_ctime in
    let check lst = List.fold_left (fun acc hd -> if hd.cdrec_uid = id then
      (curr_time -. hd.starttime >= hd.ctime) else acc) false lst in
    check team_cooldowns in
  let update_unit_health_state ct id new_health = 
    let unitdata = if ct = Red then st.red_team_data.udl else 
      st.blue_team_data.udl in
    let update_health lst = List.fold_left (fun acc hd -> 
      if hd.udrec_uid = id then hd.udrec_h <- new_health else acc) () lst in
    update_health unitdata in
  let update_unit_health_gui id new_health = 
    Netgraphics.add_update (UpdateUnit (id,new_health)) in
  let update_building_health_state ct id new_health =
    let buildingdata = if ct = Red then st.red_team_data.bdl else 
      st.blue_team_data.bdl in
    let update_health lst = List.fold_left (fun acc hd -> 
      if hd.bdrec_bi = id then hd.bdrec_h <- new_health else acc) () lst in
    update_health buildingdata in
  let update_building_health_gui id new_health = 
    Netgraphics.add_update (UpdateBuilding (id,new_health)) in
  let update_cooldowns ca id =
    let team_ctime = if ca = Red then st.red_ctime else st.blue_ctime in
    let new_ctime = get_unit_type_cooldown (get_unit_type st ca id) in
    let update lst = List.fold_left (fun acc hd -> if hd.cdrec_uid = id then
      (hd.ctime <- new_ctime; hd.starttime <- curr_time) else acc) () lst
    in update team_ctime in
  let get_new_health_unit attacker_type target_color target_id =
    let stab_bonus =
      match (attacker_type, (get_unit_type st target_color target_id)) with
        ((Archer|EliteArcher),(Pikeman|ElitePikeman)) |
        ((Knight|EliteKnight),(Archer|EliteArcher)) |
        ((Pikeman|ElitePikeman),(Knight|EliteKnight)) -> cSTAB_BONUS
        | _ -> 0 in
    let health_lost = get_unit_type_attack_damage attacker_type in
    let current_health = get_unit_health st target_color target_id in
    let new_health = current_health - health_lost - stab_bonus in
    if new_health < 0 then 0 else new_health in
  let get_new_health_building attacker_type target_color target_id =
    let health_lost = get_unit_type_attack_damage attacker_type in
    let current_health = get_building_health st target_color target_id in
    let new_health = current_health - health_lost in
    if new_health < 0 then 0 else new_health in
  let traverse_attacks lst ca ct = List.fold_left 
    (fun acc hd ->
      if (check_not_coolingdown ca hd.aqueue_uid) then
        (let pos_of_attacker = get_unit_pos st ca hd.aqueue_uid in
        let type_of_attacker = get_unit_type st ca hd.aqueue_uid in
        (find_first_attack st ct type_of_attacker pos_of_attacker 
         hd.attacks);
        if (Queue.length hd.attacks) > 0 then
          (Netgraphics.add_update (DoAttack (hd.aqueue_uid));
          match (Queue.pop hd.attacks) with
            Building b ->
              let new_health = get_new_health_building type_of_attacker ct b in
                update_building_health_state ct b new_health;
                update_building_health_gui b new_health; update_cooldowns ca
                hd.aqueue_uid    
            | Unit u ->
                let new_health = get_new_health_unit type_of_attacker ct u in
                  update_unit_health_state ct u new_health;
                  update_unit_health_gui u new_health; update_cooldowns ca
                  hd.aqueue_uid)
        else ()) else acc) () lst in 
  (traverse_attacks st.red_attack Red Blue);
  (traverse_attacks st.blue_attack Blue Red)

(* For handleTime - Removes dead units and buildings from state and gui 
 * Updates and sends score to gui *)
let remove_dead_units_and_buildings (st:state) : unit =
  let remove_dead_units_gui lst =
    let dead_units = List.filter (fun x -> x.udrec_h = 0) lst in
    let total_dead = List.length dead_units in
    let remove_units = List.fold_left (fun acc hd ->
      Netgraphics.add_update (RemoveUnit hd.udrec_uid)) () in
    remove_units dead_units; total_dead in 
  let remove_dead_units_ctime lst dead_id =
    List.fold_left (fun acc hd -> if hd.cdrec_uid = dead_id then acc else 
    hd::acc) [] lst in
  let remove_dead_units_moves lst dead_id =
    List.fold_left (fun acc hd -> if hd.mqueue_uid = dead_id then acc 
    else hd::acc) [] lst in
  let remove_dead_units_attacks lst dead_id =
    List.fold_left (fun acc hd -> if hd.aqueue_uid = dead_id then acc 
    else hd::acc) [] lst in
  let remove_dead_units_udl c lst =
    List.fold_left (fun acc hd -> if hd.udrec_h = 0 then 
    ((if c = Red then
       (st.red_ctime <- remove_dead_units_ctime st.red_ctime hd.udrec_uid;
        st.red_move <- remove_dead_units_moves st.red_move hd.udrec_uid;
        st.red_attack <- remove_dead_units_attacks st.red_attack hd.udrec_uid)
      else
        (st.blue_ctime <- remove_dead_units_ctime st.blue_ctime hd.udrec_uid;
        st.blue_move <- remove_dead_units_moves st.blue_move hd.udrec_uid;
        st.blue_attack <-remove_dead_units_attacks st.blue_attack hd.udrec_uid)
     ) ;acc)
       else hd::acc) [] lst in
  let remove_dead_buildings_gui lst =
    let dead_buildings = List.filter (fun x -> x.bdrec_h = 0) lst in
    let total_dead = List.length dead_buildings in
    let remove_buildings = List.fold_left (fun acc hd ->
      Netgraphics.add_update (RemoveBuilding hd.bdrec_bi)) () in
    remove_buildings dead_buildings; total_dead in
  let remove_dead_buildings_state = 
    List.fold_left (fun acc hd -> if hd.bdrec_h = 0 then acc else hd::acc) [] in
  st.blue_team_data.s <- (st.blue_team_data.s + 
    (remove_dead_units_gui st.red_team_data.udl) * cKILL_UNIT_SCORE); 
  st.red_team_data.s <- (st.red_team_data.s + 
    (remove_dead_units_gui st.blue_team_data.udl) * cKILL_UNIT_SCORE); 
  st.red_team_data.udl <- remove_dead_units_udl Red st.red_team_data.udl;
  st.blue_team_data.udl <- remove_dead_units_udl Blue st.blue_team_data.udl; 
  st.blue_team_data.s <- st.blue_team_data.s + 
    (remove_dead_buildings_gui st.red_team_data.bdl) * cKILL_BUILDING_SCORE;
  st.red_team_data.s <- st.red_team_data.s + 
    (remove_dead_buildings_gui st.blue_team_data.bdl) * cKILL_BUILDING_SCORE; 
  st.red_team_data.bdl <- remove_dead_buildings_state st.red_team_data.bdl;
  st.blue_team_data.bdl <- remove_dead_buildings_state st.blue_team_data.bdl;
  Netgraphics.add_update (UpdateScore (Red, (st.red_team_data.s)));
  Netgraphics.add_update (UpdateScore (Blue, (st.blue_team_data.s))) 

(* For handleTime - removes all zero resources from state and gui *)
let remove_zero_resources (st:state) : unit =
  let remove_from_gui lst = 
    let zero_resources = List.filter (fun x -> x.i = 0) lst in
    let remove_resources = List.fold_left (fun acc hd ->
      Netgraphics.add_update (RemoveResource hd.rdrec_t)) () in
    remove_resources zero_resources in
  let remove_from_state = List.fold_left (fun acc hd -> if hd.i = 0 then
    acc else hd::acc) [] in
  remove_from_gui st.resource_list;
  st.resource_list <- remove_from_state st.resource_list

(* For handleTime - checks cooldown, if the building is complete then set the
 * Villager to not building, and add the building to the building list in state
 * and the gui *)
let handle_building_creation (st:state) (curr_time:float) : unit =
  let check_if_complete building = building.building &&
    (curr_time -. building.starttime) >= building.ctime in
  let create_in_gui_and_state lst c =
    let buildingdata = if c = Red then st.red_team_data.bdl else
st.blue_team_data.bdl in
    List.fold_left (fun (acc_ctime, acc_bdl) hd -> 
      if check_if_complete hd then
       ( let new_id = next_available_id() in
        let new_tile = tile_of_pos (get_unit_pos st c hd.cdrec_uid) in
        let new_type = Barracks in
        let new_health = cBARRACKS_HEALTH in
        let age_status = if c = Red then st.red_team_data.a else
          st.blue_team_data.a in
        Netgraphics.add_update (AddBuilding (new_id, new_type, new_tile,
        new_health, c)); (if age_status = ImperialAge then
          (Netgraphics.add_update (UpgradeAge c)) else ());
        (hd.building <- false; hd.starttime <- (-1.); hd.ctime <- (-1.));
        ((({cdrec_uid=new_id; ctime=(-1.); starttime=(-1.); building=false})::hd
        ::acc_ctime), (({bdrec_bi=new_id; bdrec_bt=new_type; bdrec_h=new_health;
        bdrec_t=new_tile})::acc_bdl)) )
      else (hd::acc_ctime, acc_bdl)) ([], buildingdata) lst in
  let ((new_red_cooldown, new_red_building),
       (new_blue_cooldown, new_blue_building)) = 
    (create_in_gui_and_state st.red_ctime Red,
     create_in_gui_and_state st.blue_ctime Blue) in
  st.red_ctime <- new_red_cooldown;
  st.red_team_data.bdl <- new_red_building;
  st.blue_ctime <- new_blue_cooldown;
  st.blue_team_data.bdl <- new_blue_building

(* Updates the positions of Villagers that are not cooling down, military units
 * Changes the position in the state and updates gui *)
let update_positions (st:state) (curr_time:float) : unit =
  let check_not_coolingdown c id =
    let team_cooldowns = if c = Red then st.red_ctime else st.blue_ctime in
    List.fold_left (fun acc hd -> if hd.cdrec_uid = id then
      (curr_time -. hd.starttime >= hd.ctime) else acc) false team_cooldowns in
  let find_first_move moves_rec c =
    let utype = get_unit_type st c moves_rec.mqueue_uid in
    let uspeed = get_unit_type_speed utype in
    let curr_pos = get_unit_pos st c moves_rec.mqueue_uid in
    let rec helper queue =
      if (Queue.length queue = 0) || ((distance curr_pos (Queue.peek queue)) <= 
        uspeed +. 1.) then (Queue.length queue)
      else (ignore(Queue.pop queue); helper queue) in
    helper moves_rec.moves in
  let update_state c id new_pos =
    let unitdata = if c = Red then st.red_team_data.udl else
      st.blue_team_data.udl in
    List.fold_left (fun acc hd -> if hd.udrec_uid = id then hd.udrec_pos <- 
      new_pos else acc) () unitdata in
  let update_gui c id new_pos =
    Netgraphics.add_update (MoveUnit (id, [new_pos], c)) in
  let update_state_and_gui lst c =
    List.fold_left (fun acc hd ->
      if (check_not_coolingdown c hd.mqueue_uid) then
        (let length = find_first_move hd c in 
          (if length > 0
           then (let new_pos = Queue.pop hd.moves in
           update_state c hd.mqueue_uid new_pos; update_gui c hd.mqueue_uid
           new_pos) else ()))
      else ()) () lst in
   (update_state_and_gui st.red_move Red);
    (update_state_and_gui st.blue_move Blue) 
  
    
           

