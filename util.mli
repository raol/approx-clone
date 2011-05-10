(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)
 
val invalid_string_arg : string -> string -> 'a

(* Check if the first string is a prefix of the second *)

val is_prefix : string -> string -> bool

(* Extract substring s.[from] .. s.[until-1] *)

val substring : ?from:int -> ?until:int -> string -> string

(* Split a string at each occurrence of a separator *)

val split : char -> string -> string list

(* Join a list of strings with a separator (inverse of split) *)

val join : char -> string list -> string

(* Split a string into lines *)

val split_lines : string -> string list

(* Split a pathname into a list of components.
   Initial and final "/" map to empty strings;
   "/" by itself maps to [""; ""] *)

val explode_path : string -> string list

(* Inverse of explode_path *)

val implode_path : string list -> string

(* Infix operator to concatenate two pathname components *)

val (^/) : string -> string -> string

(* Create a directory, including any intermediate directories
   along the specified path (like "mkdir --parents") *)

val make_directory : string -> unit

(* Return a quoted string *)

val quoted_string : string -> string

(* Return the relative portion of a pathname *)

val relative_path : string -> string

(* Return the relative portion of a URL *)

val relative_url : string -> string

(* Split a filename into the leading portion without an extension
   and the extension, if any, beginning with '.' *)

val split_extension : string -> string * string

(* Return a filename with its extension, if any, removed *)

val without_extension : string -> string

(* Return the extension of a filename, including the initial '.' *)

val extension : string -> string

(* Return the underlying value of an option, otherwise raise Not_found *)

val the : 'a option -> 'a

(* Call a function making sure that a cleanup procedure is called
   before returning the result of the function or raising an exception *)

val unwind_protect : (unit -> 'a) -> (unit -> unit) -> 'a

(* Apply a function to a resource that is acquired and released by
   the given functions *)

val with_resource : ('t -> unit) -> ('a -> 't) -> 'a -> ('t -> 'b) -> 'b

(* Open an input channel and apply a function to the channel,
   using unwind_protect to ensure that the channel gets closed *)

val with_in_channel : ('a -> in_channel) -> 'a -> (in_channel -> 'b) -> 'b

(* Open an output channel and apply a function to the channel,
   using unwind_protect to ensure that the channel gets closed *)

val with_out_channel : ('a -> out_channel) -> 'a -> (out_channel -> 'b) -> 'b

(* Spawn a shell command and apply a function to its output,
   using unwind_protect to ensure that the channel gets closed *)

val with_process : ?error:string -> string -> (in_channel -> 'a) -> 'a

(* Generate a unique string, suitable for use as a filename *)

val gensym : string -> string

(* Return the name of the temporary file directory *)

val tmp_dir : unit -> string

(* Attempt to remove a file but ignore any errors *)

val rm : string -> unit

(* Check if a file is compressed by examining its extension *)

val is_compressed : string -> bool

(* Decompress a file to a temporary location and return
   the temporary file name, which must be removed by the caller *)

val decompress : string -> string

(* Decompress a file and apply a function to the temporary file name,
   using unwind_protect to ensure that the temporary file gets removed *)

val with_decompressed : string -> (string -> 'a) -> 'a

(* Apply a function to a file or to a temporary decompressed version of it *)

val decompress_and_apply : (string -> 'a) -> string -> 'a

(* Return a list of possible compressed versions of the given file *)

val compressed_versions : string -> string list

(* Return the newest file in a list, or raise Not_found if none exist *)

val newest_file : string list -> string

(* Open a file for input, decompressing it if necessary *)

val open_file : string -> in_channel

(* Open a file for exclusive output *)

val open_out_excl : string -> out_channel

(* Copy an input channel to an output channel *)

val copy_channel : in_channel -> out_channel -> unit

(* Open a temporary file for output in the same directory as the given one
   (so that it can be renamed back to the original), apply the given function,
   and return the file name *)

val with_temp_file : string -> (out_channel -> unit) -> string

(* Update the ctime of the given file, if it exists,
   without changing its access or modification times *)

val update_ctime : string -> unit

(* Check if a filename exists and is a directory *)

val directory_exists : string -> bool

(* Check if a filename is a symbolic link *)

val is_symlink : string -> bool

(* Create a generic iterator function from a fold function *)

val iter_of_fold : ((unit -> 'a) -> unit -> 'b) -> 'a -> 'b

(* Fold a function over each directory below a given path *)

val fold_dirs : ('a -> string -> 'a) -> 'a -> string -> 'a

val iter_dirs : (string -> unit) -> string -> unit

(* Fold a function over each non-directory below a given path *)

val fold_non_dirs : ('a -> string -> 'a) -> 'a -> string -> 'a

val iter_non_dirs : (string -> unit) -> string -> unit

(* Return the Unix stat information *)

val stat_file : string -> Unix.LargeFile.stats option

(* Check if a file is a cached "file not found" *)

val is_cached_nak : string -> bool

(* Return the modification time of a file *)

val file_modtime : string -> float

(* Return the status change time of a file *)

val file_ctime : string -> float

(* Calculate the age in minutes of a timestamp *)

val minutes_old : float -> int

(* Return the size of a file *)

val file_size : string -> int64

(* Return the MD5 digest of a file *)

val file_md5sum : string -> string

(* Return the SHA1 digest of a file *)

val file_sha1sum : string -> string

(* Return the SHA256 digest of a file *)

val file_sha256sum : string -> string

(* Drop privileges to those of the given user and group *)

val drop_privileges : user:string -> group:string -> unit

(* Check that the program is executing as the given user and group *)

val check_id : user:string -> group:string -> unit

(* Convert a socket address to a string *)

val string_of_sockaddr : Unix.sockaddr -> with_port:bool -> string
