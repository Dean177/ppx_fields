module Form = struct
  type state = {
    email: string;
    age: int;
  } [@@deriving fields]
end
