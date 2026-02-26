open Ezjs_min

module Types = Ezjs_idb_types
open Types

type mode = READONLY | READWRITE | VERSIONCHANGE
type direction = NEXT | PREV | NEXTUNIQUE | PREVUNIQUE

type db_options = { key_path : string option; auto_increment : bool option }

type db_upgrade = { old_version : int; new_version : int }

type index_options = { unique : bool option; multi_entry : bool option; locale : bool option }

module type Tr_sig = sig
  type t
  type js
  val to_js : t -> js
  val of_js : js -> t
end

let str_of_mode = function
  | READONLY -> string "readonly"
  | READWRITE -> string "readwrite"
  | VERSIONCHANGE -> string "versionchange"

let str_of_direction = function
  | NEXT -> string "next"
  | PREV -> string "prev"
  | NEXTUNIQUE -> string "nextunique"
  | PREVUNIQUE -> string "prevunique"

let result r = r##.result

let catch exn = function
  | None -> raise exn
  | Some f -> catch_exn (fun e -> f (AOpt.def (Unsafe.coerce e))) exn

let wrap ?error ?callback (r : _ iDBRequest t) =
  try
    r##.onsuccess := AOpt.aopt (fun f -> wrap_callback (fun _e ->
        try f (result r) with exn -> catch exn error)) callback;
    r##.onerror := AOpt.aopt (fun f -> wrap_callback (fun _e -> f r##.error)) error
  with exn -> catch exn error

let db_upgrade_event (e : iDBVersionChangeEvent t) = {
  old_version = e##.oldVersion;
  new_version = e##.newVersion;
}

let openDB ?upgrade ?error ?version name callback =
  let indexedDB : _ iDBFactory t = Unsafe.global##._indexedDB in
  let r = indexedDB##_open (string name) (AOpt.option version) in
  wrap ?error ~callback (r :> _ iDBRequest t);
  r##.onupgradeneeded :=
    AOpt.aopt (fun u -> wrap_callback (fun e ->
        let v = db_upgrade_event e in
        let db = result r in
        u db v)) upgrade

let transaction r : iDBTransaction t = r##.transaction

let create_db_options {key_path; auto_increment} : create_db_options t = object%js
  val keyPath = AOpt.aopt string key_path
  val autoIncrement = AOpt.aopt bool auto_increment
end

let create_store ?options (db : iDBDatabase t) name =
  Unsafe.coerce @@
  db##createObjectStore (string name) (AOpt.aopt create_db_options options)

let create_transaction ?mode (db : iDBDatabase t) names =
  let names = of_listf string names in
  db##transaction names (AOpt.aopt str_of_mode mode)

let get_store ?mode ?tx (db : iDBDatabase t) name : (_, _) iDBObjectStore t =
  let tx = match tx with
    | None -> create_transaction ?mode db [ name ]
    | Some tx -> tx in
  Unsafe.coerce @@ tx##objectStore (string name)

type error_cb = domException t aopt -> unit

module type R = sig
  type k
  type d
  type store = (k, d) iDBObjectStore t
  type keys = K of k | KR of k iDBKeyRange t
  val add : ?error:error_cb -> ?callback:(k -> unit) -> ?key:k -> store -> d -> unit
  val put : ?error:error_cb -> ?callback:(k -> unit) -> ?key:k -> store -> d -> unit
  val range : ?olower:bool -> ?oupper:bool -> ?lower:k -> ?upper:k -> unit -> keys
  val count : ?error:error_cb -> ?key:keys -> store -> (int -> unit) -> unit
  val get : ?error:error_cb -> store -> (d aopt -> unit) -> keys -> unit
  val get_all : ?error:error_cb -> ?key:keys -> ?count:int -> store -> (d js_array t -> unit) -> unit
  val get_key : ?error:error_cb -> store -> (k aopt -> unit) -> keys -> unit
  val get_all_keys : ?error:error_cb -> ?key:keys -> ?count:int -> store -> (k js_array t -> unit) -> unit
  val delete : ?error:error_cb -> ?callback:(unit aopt -> unit) -> store -> keys -> unit
  val iter : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (k -> d -> unit) -> unit
  val fold : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (k -> d -> 'a -> 'a) -> 'a -> ('a -> unit) -> unit
  val iter_keys : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (k -> unit) -> unit
  val fold_keys : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (k -> 'a -> 'a) -> 'a -> ('a -> unit) -> unit
end

module type S = sig
  module K : Tr_sig
  module D : Tr_sig
  module Raw : R with type k = K.js and type d = D.js
  type store = Raw.store
  type keys = K of K.t | KR of K.js iDBKeyRange t
  val name : unit -> string
  val set_name : string -> unit
  val create : ?options:db_options -> ?name:string -> iDBDatabase t -> store
  val store : ?mode:mode -> ?tx:iDBTransaction t -> ?name:string -> iDBDatabase t -> store
  val add : ?error:error_cb -> ?callback:(K.t -> unit) -> ?key:K.t -> store -> D.t -> unit
  val put : ?error:error_cb -> ?callback:(K.t -> unit) -> ?key:K.t -> store -> D.t -> unit
  val range : ?olower:bool -> ?oupper:bool -> ?lower:K.t -> ?upper:K.t -> unit -> keys
  val count : ?error:error_cb -> ?key:keys -> store -> (int -> unit) -> unit
  val get : ?error:error_cb -> store -> (D.t option -> unit) -> keys -> unit
  val get_all : ?error:error_cb -> ?key:keys -> ?count:int -> store -> (D.t list -> unit) -> unit
  val get_key : ?error:error_cb -> store -> (K.t option -> unit) -> keys -> unit
  val get_all_keys : ?error:error_cb -> ?key:keys -> ?count:int -> store -> (K.t list -> unit) -> unit
  val delete : ?error:error_cb -> ?callback:(unit option -> unit) -> store -> keys -> unit
  val iter : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (K.t -> D.t -> unit) -> unit
  val fold : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (K.t -> D.t -> 'a -> 'a) -> 'a -> ('a -> unit) -> unit
  val iter_keys : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (K.t -> unit) -> unit
  val fold_keys : ?error:error_cb -> ?key:keys -> ?direction:direction -> store -> (K.t -> 'a -> 'a) -> 'a -> ('a -> unit) -> unit
  val clear : ?error:error_cb -> ?callback:(unit -> unit) -> store -> unit
  val create_index_options : index_options -> create_index_options t
  val create_index : ?options:index_options -> name:string -> key_path:string -> store -> (K.js, D.js) iDBIndex t
  val delete_index : store -> string -> unit
  val get_index : store -> string -> (K.js, D.js) iDBIndex t
end

module Store(K : Tr_sig)(D : Tr_sig) : S with
  type K.js = K.js and type K.t = K.t and type D.js = D.js and type D.t = D.t
= struct
  module K = K
  module D = D

  module Raw = struct
    type k = K.js
    type d = D.js
    type store = (k, d) iDBObjectStore t
    type keys = K of k | KR of k iDBKeyRange t

    let add ?error ?callback ?key (st : store) (x : d) =
      wrap ?callback ?error @@ st##add x (AOpt.option key)

    let put ?error ?callback ?key (st : store) (x : d) =
      wrap ?callback ?error @@ st##put x (AOpt.option key)

    let range ?olower ?oupper ?lower ?upper () =
      let iDBKeyRange : k iDBKeyRange t = Unsafe.global##._IDBKeyRange in
      match lower, upper with
      | None, None -> assert false
      | Some lower, None ->
        KR (iDBKeyRange##lowerBound lower (AOpt.aopt bool olower))
      | None, Some upper ->
        KR (iDBKeyRange##upperBound upper (AOpt.aopt bool oupper))
      | Some lower, Some upper ->
        KR (iDBKeyRange##bound
              lower upper (AOpt.aopt bool olower) (AOpt.aopt bool oupper))

    let count ?error ?key (st : store) (callback : int -> unit) =
      match key with
      | None -> wrap ~callback ?error @@ st##count AOpt.undefined
      | Some (K k) -> wrap ~callback ?error @@ st##count (AOpt.def k)
      | Some (KR r) -> wrap ~callback ?error @@ st##count_range (AOpt.def r)

    let get ?error (st : (k, d) iDBObjectStore t) (callback : d aopt -> unit) k =
      match k with
      | K key -> wrap ~callback ?error @@ st##get key
      | KR range -> wrap ~callback ?error @@ st##get_range range

    let get_all ?error ?key ?count (st : (k, d) iDBObjectStore t) (callback : d js_array t -> unit) =
      match key with
      | None -> wrap ~callback ?error @@ st##getAll AOpt.undefined (AOpt.option count)
      | Some (K k) -> wrap ~callback ?error @@ st##getAll (AOpt.def k) (AOpt.option count)
      | Some (KR r) -> wrap ~callback ?error @@ st##getAll_range (AOpt.def r) (AOpt.option count)

    let get_key ?error (st : (k, _) iDBObjectStore t) (callback : k aopt -> unit) k =
      match k with
      | K key -> wrap ~callback ?error @@ st##getKey key
      | KR range -> wrap ~callback ?error @@ st##getKey_range range

    let get_all_keys ?error ?key ?count (st : (k, _) iDBObjectStore t) (callback : k js_array t -> unit) =
      match key with
      | None -> wrap ~callback ?error @@ st##getAllKeys AOpt.undefined (AOpt.option count)
      | Some (K k) -> wrap ~callback ?error @@ st##getAllKeys (AOpt.def k) (AOpt.option count)
      | Some (KR r) -> wrap ~callback ?error @@ st##getAllKeys_range (AOpt.def r) (AOpt.option count)

    let delete ?error ?callback (st : (k, _) iDBObjectStore t) = function
      | K key -> wrap ?error ?callback @@ st##delete key
      | KR range -> wrap ?error ?callback @@ st##delete_range range

    let iter ?error ?key ?direction (st : (k, d) iDBObjectStore t)
        (f : k -> d -> unit) =
      let callback c =
        match AOpt.to_option c with
        | None -> ()
        | Some c ->
          match AOpt.to_option c##.key with
          | Some k ->
            f k c##.value;
            c##continue AOpt.undefined
          | None -> c##continue AOpt.undefined in
      match key with
      | None -> wrap ~callback ?error @@
        st##openCursor AOpt.undefined (AOpt.aopt str_of_direction direction)
      | Some (K k) -> wrap ~callback ?error @@
        st##openCursor (AOpt.def k) (AOpt.aopt str_of_direction direction)
      | Some (KR r) -> wrap ~callback ?error @@
        st##openCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction)

    let fold ?error ?key ?direction (st : (k, d) iDBObjectStore t)
        (f : k -> d -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
      let acc = ref start in
      let callback c =
        match AOpt.to_option c with
        | None -> callback !acc
        | Some c ->
          match AOpt.to_option c##.key with
          | Some k ->
            acc := f k c##.value !acc;
            c##continue AOpt.undefined
          | None -> c##continue AOpt.undefined in
      match key with
      | None -> wrap ~callback ?error @@
        st##openCursor AOpt.undefined (AOpt.aopt str_of_direction direction)
      | Some (K k) -> wrap ~callback ?error @@
        st##openCursor (AOpt.def k) (AOpt.aopt str_of_direction direction)
      | Some (KR r) -> wrap ~callback ?error @@
        st##openCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction)

    let iter_keys ?error ?key ?direction (st : (k, d) iDBObjectStore t)
        (f : k -> unit) =
      let callback c =
        match AOpt.to_option c with
        | None -> ()
        | Some c ->
          match AOpt.to_option c##.key with
          | Some k ->
            f k;
            c##continue AOpt.undefined
          | None -> c##continue AOpt.undefined in
      match key with
      | None -> wrap ~callback ?error @@
        st##openKeyCursor AOpt.undefined (AOpt.aopt str_of_direction direction)
      | Some (K k) -> wrap ~callback ?error @@
        st##openKeyCursor (AOpt.def k) (AOpt.aopt str_of_direction direction)
      | Some (KR r) -> wrap ~callback ?error @@
        st##openKeyCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction)

    let fold_keys ?error ?key ?direction (st : (k, d) iDBObjectStore t)
        (f : k -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
      let acc = ref start in
      let callback c =
        match AOpt.to_option c with
        | None -> callback !acc
        | Some c ->
          match AOpt.to_option c##.key with
          | Some k ->
            acc := f k !acc;
            c##continue AOpt.undefined
          | None -> c##continue AOpt.undefined in
      match key with
      | None -> wrap ~callback ?error @@
        st##openKeyCursor AOpt.undefined (AOpt.aopt str_of_direction direction)
      | Some (K k) -> wrap ~callback ?error @@
        st##openKeyCursor (AOpt.def k) (AOpt.aopt str_of_direction direction)
      | Some (KR r) -> wrap ~callback ?error @@
        st##openKeyCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction)
  end

  type store = (K.js, D.js) iDBObjectStore t
  type keys = K of K.t | KR of K.js iDBKeyRange t

  let name = ref "store"
  let set_name s = name := s

  let create ?options ?(name= !name) db : store =
    create_store ?options db name

  let store ?mode ?tx ?(name= !name) db : store =
    get_store ?mode ?tx db name

  let add ?error ?callback ?key (st : store) (x : D.t) =
    let callback = Option.map (fun f -> (fun k -> f (K.of_js k))) callback in
    let key = Option.map K.to_js key in
    Raw.add ?error ?callback ?key st (D.to_js x)

  let put ?error ?callback ?key (st : store) (x : D.t) =
    let callback = Option.map (fun f -> (fun k -> f (K.of_js k))) callback in
    let key = Option.map K.to_js key in
    Raw.put ?error ?callback ?key st (D.to_js x)

  let range ?olower ?oupper ?lower ?upper () =
    let lower, upper = Option.map K.to_js lower, Option.map K.to_js upper in
    match Raw.range ?olower ?oupper ?lower ?upper () with
    | Raw.KR k -> KR k
    | Raw.K k -> K (K.of_js k)

  let count ?error ?key (st : store) (callback : int -> unit) =
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    Raw.count ?error ?key st callback

  let get ?error (st : (K.js, D.js) iDBObjectStore t) (callback : D.t option -> unit) k =
    let callback o = callback (AOpt.to_aopt D.of_js o) in
    let k = match k with K k -> Raw.K (K.to_js k) | KR k -> Raw.KR k in
    Raw.get ?error st callback k

  let get_all ?error ?key ?count (st : (K.js, D.js) iDBObjectStore t) (callback : D.t list -> unit) =
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    let callback a = callback (to_listf D.of_js a) in
    Raw.get_all ?error ?key ?count st callback

  let get_key ?error (st : (K.js, _) iDBObjectStore t) (callback : K.t option -> unit) k =
    let callback o = callback (AOpt.to_aopt K.of_js o) in
    let k = match k with K k -> Raw.K (K.to_js k) | KR k -> Raw.KR k in
    Raw.get_key ?error st callback k

  let get_all_keys ?error ?key ?count (st : (K.js, _) iDBObjectStore t) (callback : K.t list -> unit) =
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    let callback a = callback (to_listf K.of_js a) in
    Raw.get_all_keys ?error ?key ?count st callback

  let delete ?error ?callback (st : (K.js, _) iDBObjectStore t) k =
    let k = match k with K k -> Raw.K (K.to_js k) | KR k -> Raw.KR k in
    let callback = Option.map (fun f -> (fun o -> f (AOpt.to_option o))) callback in
    Raw.delete ?error ?callback st k

  let iter ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t) (f : K.t -> D.t -> unit) =
    let f k d = f (K.of_js k) (D.of_js d) in
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    Raw.iter ?error ?key ?direction st f

  let fold ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> D.t -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
    let f k d acc = f (K.of_js k) (D.of_js d) acc in
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    Raw.fold ?error ?key ?direction st f start callback

  let iter_keys ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> unit) =
    let f k = f (K.of_js k) in
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    Raw.iter_keys ?error ?key ?direction st f

  let fold_keys ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
    let f k acc = f (K.of_js k) acc in
    let key = match key with Some K k -> Some (Raw.K (K.to_js k)) | None -> None | Some KR k -> Some (Raw.KR k) in
    Raw.fold_keys ?error ?key ?direction st f start callback

  let clear ?error ?callback (st : (K.js, D.js) iDBObjectStore t) =
    let callback = Option.map (fun f -> (fun _ -> f ())) callback in
    wrap ?callback ?error @@ st##clear

  let create_index_options {unique; multi_entry; locale} : create_index_options t = object%js
    val unique = AOpt.aopt bool unique
    val multiEntry = AOpt.aopt bool multi_entry
    val locale = AOpt.aopt bool locale
  end

  let create_index ?options ~name ~key_path (st : (K.js, D.js) iDBObjectStore t) =
    let options = AOpt.aopt create_index_options options in
    st##createIndex (string name) (string key_path) options

  let delete_index (st : (K.js, D.js) iDBObjectStore t) name =
    ignore @@ st##deleteIndex (string name)

  let get_index (st : (K.js, D.js) iDBObjectStore t) name =
    st##index (string name)

  let name () = !name
end

module NoTr(S : sig type t end) : Tr_sig with type t = S.t and type js = S.t = struct
  type t = S.t
  type js = S.t
  let to_js x = x
  let of_js x = x
end

module IntTr : (Tr_sig with type js = int and type t = int) = NoTr(struct type t = int end)

module StringTr : Tr_sig with type js = js_string t and type t = string = struct
  type js = js_string t
  type t = string
  let to_js x = string x
  let of_js x = to_string x
end

module NumberTr : Tr_sig with type js = number t and type t = float = struct
  type js = number t
  type t = float
  let to_js x = number_of_float x
  let of_js x = float_of_number x
end

module DateTr : Tr_sig with type js = date t and type t = string = struct
  type js = date t
  type t = string
  let to_js x = new%js date_fromTimeValue (date##parse (string x))
  let of_js x = to_string x##toLocaleString
end
