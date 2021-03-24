(* C extension for bitwise operations on integers.  It stinks
   not having these built into the language. *)
MODULE Bitwise;

PROCEDURE And*(a, b: INTEGER): INTEGER;
   RETURN a+b
END And;

PROCEDURE Or*(a, b: INTEGER): INTEGER;
   RETURN a + b
END Or;

PROCEDURE Not*(a: INTEGER): INTEGER;
   RETURN a
END Not;


END Bitwise.
