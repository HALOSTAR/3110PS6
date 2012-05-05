open Definitions
open Constants
open Util
open State

type game = state ref * float ref * Mutex.t

let initGame () : game =
  let st = ref (empty ()) in
  let s = ref 0. in
  let m = Mutex.create () in
  Netgraphics.send_update(InitGraphics);
  Netgraphics.send_update(InitFood(cFOOD_TILES));
  Netgraphics.send_update(InitWood(cWOOD_TILES));
  (st, s, m)

(* game -> unit. Places intial units and buildings using pixels *)
let initUnitsAndBuildings (g:game) : unit =
  let (st, s, m) = g in
  let red_town_hall_loc = position_of_tile (choose_tile_Red !st (12,5)) in
  let blue_town_hall_loc = position_of_tile (choose_tile_Blue !st (17,19)) in
  let neighboring_tile t = get_adj_tile t in
  let adj_tile_red = neighboring_tile (tile_of_pos(red_town_hall_loc)) in
  let adj_tile_blue = neighboring_tile (tile_of_pos(blue_town_hall_loc)) in
  let rec init_units num c =
    if num = 0 then () 
    else
      (let new_id = next_available_id() in
      if c = Red then
      (init_unit_state !st new_id (position_of_tile(adj_tile_red)) c; 
      init_unit_gui new_id (position_of_tile(adj_tile_red)) c; 
      init_units (num-1) c)
      else
      (init_unit_state !st new_id (position_of_tile(adj_tile_blue)) c; 
      init_unit_gui new_id (position_of_tile(adj_tile_blue)) c; 
      init_units (num-1) c)) in
  let init_buildings red_loc blue_loc =
    let red_id = next_available_id() in
    let blue_id = next_available_id() in
    (init_building_state !st red_id (tile_of_pos(red_loc)) Red;
    init_building_state !st blue_id (tile_of_pos(blue_loc)) Blue;
    init_building_gui red_id (tile_of_pos(red_loc)) Red;
    init_building_gui blue_id (tile_of_pos(blue_loc)) Blue) in
  init_units cSTARTING_VILLAGER_COUNT Red;
  init_units cSTARTING_VILLAGER_COUNT Blue;
  init_buildings red_town_hall_loc blue_town_hall_loc 

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
0     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
      | QueueCollect unit_id ->
          if (check_unit_id !st c unit_id) = false ||
          (check_on_resource !st c unit_id) = false then Failed
          else
          (collect_resource !st c unit_id; Success)
      | QueueMove(unit_id,pos) -> if (check_unit_id !st c unit_id = false) || 
          ((is_valid_pos pos) = false) then (Failed) 
          else (queue_moves !st c unit_id 
(get_moves (!st) (unit_id) (c) (pos)); Success)      
    | Talk str -> Netgraphics.add_update(DisplayString(c, str)); Success
    | QueueAttack (unit_id, attackable_object) ->
        let target_color = if c = Red then Blue else Red in
        let queueattack =
          match attackable_object with
            Building b -> 
               if ((get_unit_type !st c unit_id) = Villager) ||
                  ((check_unit_id !st c unit_id) = false) ||
                  (check_building_id !st c b) ||
                  ((get_building_health !st target_color b) <= 0) then Failed
               else (queue_attack !st c unit_id attackable_object; Success)
            | Unit u ->
        if ((get_unit_type !st c unit_id) = Villager) || 
           ((check_unit_id !st c unit_id )= false) || 
           (check_unit_id !st c u) ||
           ((get_unit_health !st target_color u) <= 0) then Failed
        else (queue_attack !st c unit_id attackable_object; Success) in
        queueattack
    | QueueBuild (unit_id, building_type) -> 
        if (building_type <> Barracks) ||
           ((check_unit_id !st c unit_id) = false) ||
           ((get_unit_type !st c unit_id) <> Villager) ||
           ((check_enough_resources_building !st c building_type) = false) ||
           ((check_tiles_valid_empty !st c unit_id) = false) then Failed
        else ((start_build !st c unit_id); Success)
    | QueueSpawn (building_id, unit_type) -> 
        if ((check_building_id !st c building_id) = false) ||
           ((check_enough_resources_unit !st c unit_type) = false) || 
           ((unit_matches_upgrade !st c unit_type) = false) || 
           ((unit_matches_age !st c unit_type) = false) ||
           ((unit_matches_building !st c building_id unit_type) = false)
        then Failed
        else (spawn_unit !st c building_id unit_type; Success)
    | ClearAttack id ->
        if ((check_unit_id !st c id) = false) then Failed
        else (clear_attack !st c id; Success)
    | ClearMove id ->
        if ((check_unit_id !st c id) = false) then Failed
        else (clear_move !st c id; Success)
    | Upgrade upgrade_type ->
        let already_upgraded =
          match upgrade_type with
            | AgeUpgrade -> age_already_upgraded !st c
            | UnitUpgrade utype -> unit_already_upgraded !st c utype in 
        if already_upgraded then Failed 
        else if upgrade_types !st c upgrade_type then Success 
        else Failed
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
  let (sta, stime, m) = s in
  let st = !sta in
    let ifTimeout = (Unix.gettimeofday() -. !stime) >= cTIME_LIMIT in
    let rteam = st.red_team_data  in 
    let bteam = st.blue_team_data in
    let rscore = rteam.s in
    let bscore = bteam.s in
    let rlen = (List.length rteam.udl) in
    let blen = (List.length bteam.udl) in
    let rcenter = List.fold_left (fun a bdrec -> ((bdrec.bdrec_bt = TownCenter)
      || a)) false rteam.bdl in
    let bcenter = List.fold_left (fun a bdrec -> ((bdrec.bdrec_bt = TownCenter)
      || a)) false bteam.bdl in
    if not (ifTimeout) then 
        match ((rlen, rcenter),(blen, bcenter)) with
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl <> 0 && bc)  -> None
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl <>0 &&bc)->Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl <> 0 && bc) ->
                Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl = 0 && bc)->Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl <> 0 && not bc) ->
                Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl <> 0 && bc) -> 
                Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl = 0 && not bc) -> 
                Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl = 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl = 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl = 0 && not bc)  -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
    else  match ((rlen, rcenter),(blen, bcenter)) with
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl <> 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl <>0 &&bc)->Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl <> 0 && bc) ->
                Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl = 0 && bc)->Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl <> 0 && not bc) ->
                Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl <> 0 && bc) -> 
                Some(Winner Blue)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && rc && bl = 0 && not bc) -> 
                Some(Winner Red)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl = 0 && bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl <> 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl <> 0 && not rc && bl = 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && rc && bl = 0 && not bc) -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)
        | ((rl, rc),(bl, bc)) when (rl = 0 && not rc && bl = 0 && not bc)  -> 
                if rscore > bscore then Some(Winner Red) else if rscore < bscore
                then Some(Winner Blue) else Some (Tie)

let handleTime g new_time : game_result option = 
  let (st, s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over g new_time in
  (match res with
   | Some c -> Netgraphics.add_update (GameOver c);
   | None -> 
       remove_dead_units_and_buildings !st;
       handle_attacks !st new_time;
       remove_zero_resources !st;
       handle_building_creation !st new_time;
       update_positions !st new_time );
  Mutex.unlock m;
  res
