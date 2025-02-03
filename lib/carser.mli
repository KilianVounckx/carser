(** Module Carser contains submodules {!Parser}, which defines a functor.
    Other modules are define specific instances of that functor. *)

module Parser : module type of Parser
module String : module type of String
