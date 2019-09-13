module Form =
  struct
    type state = {
      email: string;
      age: int;}
    module Fields =
      struct
        type _ fieldName =
          | Email: string fieldName
          | Age: int fieldName
        type field =
          | Field: 'a fieldName* 'a -> field
        let get : 'value . state -> 'value fieldName -> 'value= fun (type
          value) ->
          (fun state  ->
             fun fieldName  ->
               match fieldName with
               | Email  -> state.email
               | Age  -> state.age : state -> value fieldName -> value)
        let set : 'value . state -> 'value fieldName -> 'value -> state= fun
          (type value) ->
          (fun state  ->
             fun fieldName  ->
               fun value  ->
                 match fieldName with
                 | Email  -> { state with email = value }
                 | Age  -> { state with age = value } : state ->
                                                          value fieldName ->
                                                            value -> state)
        let toInt : 'value . 'value fieldName -> int= fun (type value) ->
          (fun fieldName  -> match fieldName with | Email  -> 0 | Age  -> 1 :
          value fieldName -> int)
        let compare: 'a fieldName -> 'b fieldName -> int =
          fun fieldA  ->
            fun fieldB  -> Pervasives.compare (toInt fieldA) (toInt fieldB)
        let fold:
          state ->
            initial:'result -> f:('result -> field -> 'result) -> 'result
          =
          fun state  ->
            fun ~initial  ->
              fun ~f  ->
                Array.fold_left (fun result  -> fun field  -> f result field)
                  initial
                  [|(Field (Email, (get state Email)));(Field
                                                          (Age,
                                                            (get state Age)))|]
      end
  end
