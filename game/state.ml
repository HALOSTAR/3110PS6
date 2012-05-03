open Constants
open Definitions
open Util

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
mutable red_build_ctime:cdrec list; mutable blue_build_ctime:cdrec list; 
mutable red_ctime:cdrec list; mutable blue_ctime:cdrec list;
mutable red_move:mqueue list; mutable blue_move:mqueue list; 
mutable red_attack:aqueue list; mutable blue_attack:aqueue list}

(* empty state, place food, wood units *) 
(*
let empty () : state =
  let f acc hd t c = (hd, t, c)::acc in
  let resourcelist =
    List.fold_left (fun acc1 hd1 -> f acc1 hd1 Wood cINITIAL_WOOD) 
      (List.fold_left (fun acc2 hd2 -> f acc2 hd2 Food cINITIAL_FOOD)
      [] cFOOD_TILES) cWOOD_TILES in
  (resourcelist, [], [], (0,[],[],DarkAge,0,0,(false,false,false)), 
    (0,[],[],DarkAge,0,0,(false,false,false))) *)

(* For queueCollect. Checks if c has unit id and whether the unit is a Villager
 * Returns position (-1., -1.) if id or unit check fails, the position of
 * the Villager otherwise *)
(*
let checkid_checkunit (st:state) (unitid:unit_id) (c:color) : position =
  let (r, cr, cb, tdr, tdb) = st in
  let (sr, ur, br, ar, fr, wr, upr) = tdr in
  let (sb, ub, bb, ab, fb, wb, upb) = tdb in
  let idInList lst = List.fold_left (fun (count, unittype, unitpos) hd ->
    match hd with
      (id, t, h, p) -> if id = unitid then (count+1, t, p) 
      else (count, unittype, unitpos)) 
      (0, Villager, (0.0, 0.0)) lst in
  let (rcount, rtype, rpos) = idInList ur in
  let (bcount, btype, bpos) = idInList ub in
  if (rcount = 1 && bcount = 0 && rtype = Villager) 
  then (if c = Red then rpos else (-1., -1.))
  else if (rcount = 0 && bcount = 1 && btype = Villager)
  then (if c = Blue then bpos else (-1.,-1.))
  else (-1.,-1.) *)

(* Checks if the Villager is standing on a valid resource and updates the
 * resource count and team c score. Returns true and a new state if successful,
 * false and old state otherwise *) 
(*
let update_score_resource (st:state) (pos:position) (c:color) : bool*rtype*
rcount*collected*state =
  let (r, cr, cb, tdr, tdb) = st in
  let update_resource_count lst = List.fold_left (fun (t_or_f, r_type, 
    rsdata_list) (t, rs, i) ->
    if (i >= 1) && (position_of_tile t = pos) then 
      (true, rs, (t,rs,(i-1))::rsdata_list)
    else (t_or_f, r_type, (t,rs,i)::rsdata_list)) (false, Food, []) lst in
  let update_score (s, udl, bdl, a, fc, wc, u) = if a = DarkAge then 
    ((s+cRESOURCE_COLLECTED),udl, bdl, a, fc, wc, u) else 
    ((s+cADVANCED_RESOURCE_COLLECTED),udl,bdl,a,fc,wc,u) in
  match (update_resource_count r) with
    (t_or_f, t_type, rsdata_list) -> if t_or_f then (if c = Red then (t_or_f, 
                                             (rsdata_list, cr, cb, 
                                             (update_score tdr), 
                                             tdb)) else (t_or_f, (rsdata_list, 
                                             cr, cb, tdr, (update_score 
                                             tdb)))) else

                               (t_or_f, (r, cr, cb, tdr, tdb)) *)

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
  ((x2-.x1),(y2-.y1)) in
  let unit_speed = get_unit_type_speed (get_unit_type st c id) in
  let moves_queue = Queue.create () in
  let rec enqueue_moves curr =
    if (distance (curr) (pos)) <= unit_speed then Queue.add (pos) (moves_queue)
    else 
      let (inter_posx, inter_posy) =
        let (x1, y1) = curr in
        let (x2, y2) = normalized_vector curr pos in
(x1+.(x2*.unit_speed), y1+.(y2*.unit_speed)) in
      Queue.add (inter_posx, inter_posy) (moves_queue); enqueue_moves
(inter_posx, inter_posy) in
  if (current_position = pos) then moves_queue else 
(enqueue_moves (current_position); moves_queue)

(* For QueueMove:
 * Takes: state, color, unit id, queue of moves to add
 * Returns: a new state with the moves added to the right unit *)
let queue_moves (st:state) (c:color) (id:unit_id) (moves:position Queue.t) :
state =
  let team_move_list = if c = Red then st.red_move else st.blue_move in
  let team_moves_queue lst = List.fold_left (fun acc hd -> if hd.mqueue_uid =id 
                                            then hd.moves else acc) 
(Queue.create()) lst in
  Queue.transfer (moves) (team_moves_queue team_move_list); st

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
     st.red_build_ctime) && (check_already_building st.blue_build_ctime) in
  let (tilex, tiley) = current_tile in
  (check_valid current_tile) && (check_empty (tilex,tiley) (tilex+1,tiley)
  (tilex,tiley+1) (tilex+1,tiley+1))

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
let start_build (st:state) (c:color) (id:unit_id) : state =
  let (food_cost,wood_cost) = cBARRACKS_COST in
  let team_build_list = if c = Red then st.red_build_ctime else 
st.blue_build_ctime in
  let team_data_list = if c = Red then st.red_team_data else
st.blue_team_data in
  let subtract_resources lst = lst.fc <- (lst.fc - food_cost);
lst.wc <- (lst.wc - wood_cost) in
  let update_builder lst = List.fold_left (fun acc hd -> if
    hd.cdrec_uid = id then (hd.starttime <- (Unix.gettimeofday()); hd.ctime <-
    (get_unit_type_cooldown (get_unit_type st c id)); hd.building <- true) else
     acc) () lst in
  subtract_resources (team_data_list); update_builder (team_build_list); st

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

(* For QueueSpawn - spawns the unit for the given team, updates state
 * Takes: state, color, the building for spawning, the type to spawn
 * Returns: an updated state *)
let spawn_unit (st:state) (c:color) (id:building_id) (utype:unit_type) :
unit =
  let subtract_costs = 
    let team_data = if c = Red then st.red_team_data else st.blue_team_data in
    let (food_count, wood_count) = (team_data.fc, team_data.wc) in
    let (food_cost, wood_cost) = get_unit_cost utype in
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
    health_of_spawned, c)) in
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

(* Upgrades the age for color c - Called by upgrade_types *)
let upgrade_age (st:state) (c:color) : bool =
  let team_data = if c = Red then st.red_team_data else st.blue_team_data in
  let upgrade_state = 
    let (food_cost, wood_cost) = cUPGRADE_AGE_COST in
    let (food_count, wood_count) = (team_data.fc, team_data.wc) in
  team_data.fc <- (food_count-food_cost);team_data.wc <- (wood_count-wood_cost);
  team_data.a <- ImperialAge in
  let upgrade_gui = Netgraphics.send_update (UpgradeAge (c)) in
  if (team_data.a <> ImperialAge) then (upgrade_state; upgrade_gui; true)
  else false

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
  let already_upgraded =
    match elite_version with
      ElitePikeman -> team_data.up.pikeman
      | EliteArcher -> team_data.up.archer
      | EliteKnight -> team_data.up.knight in
  let upgrade_record =
     match elite_version with
      ElitePikeman -> team_data.up.pikeman <- true
      | EliteArcher -> team_data.up.archer <- true
      | EliteKnight -> team_data.up.knight <- true in
  let check_age = unit_matches_age st c utype in
  let upgrade_state =
    let (food_cost, wood_cost) =
      match elite_version with
        ElitePikeman -> cUPGRADE_PIKEMAN_COST
        | EliteArcher -> cUPGRADE_ARCHER_COST
        | EliteKnight -> cUPGRADE_KNIGHT_COST
        | _ -> (-1, -1) in
    let (food_count, wood_count) = (team_data.fc, team_data.wc) in
    let traverse_units lst = List.fold_left (fun acc hd ->
      if hd.udrec_ut = inf_version then hd.udrec_ut <- elite_version
      else acc) () lst in
    traverse_units (team_data.udl); team_data.fc <-(food_count-food_cost); 
    team_data.wc <-(wood_count-wood_cost) in
  let upgrade_gui = Netgraphics.send_update (UpgradeUnit (inf_version, c)) in
  if (check_age) && (already_upgraded = false) then (upgrade_state;
    upgrade_gui; true) else false

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
 * Returns: Queue of attacks with the first being valid
 * Validity: target is in range, not dead *)
let rec find_first_attack (st:state) (ct:color) (utype:unit_type) (pos:position)
(attacks:attackable_object Queue.t) :
    attackable_object Queue.t =
  let target_health =
    match Queue.peek attacks with
      Building b -> get_building_health st ct b
      | Unit u -> get_unit_health st ct u in
  let check_range =
    let targetpos = 
      match Queue.peek attacks with
        Building b -> position_of_tile (get_building_tile st ct b)
        | Unit u -> get_unit_pos st ct u in
    (length_of_range (get_unit_type_range utype)) >= (distance pos targetpos) in
  if target_health > 0 && check_range then attacks
  else if (Queue.length attacks) = 1 then (Queue.create())
  else (ignore(Queue.pop attacks); find_first_attack st ct utype pos attacks)
    

(* For handleTime - finds (if any) valid attacks for each unit and removes
 * them from the attack queue. Also subtracts health from targets. Finally,
 * updates the GUI *)  
let handle_attacks (st:state) : unit =
  let red_attack_queue = st.red_attack in
  let blue_attack_queue = st.blue_attack in
  let traverse_attacks lst c = List.fold_left 
    (fun acc hd ->
       let pos_of_attacker = get_unit_pos st c hd.aqueue_uid in
       let range_of_attacker = get_unit_type st c hd.aqueue_uid in
       let 
