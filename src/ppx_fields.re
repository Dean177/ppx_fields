open Migrate_parsetree;
open OCaml_406.Ast;

open Ast_mapper;
open Asttypes;
open Parsetree;
open Ast_helper;

let loc = Location.none;

let gadtFieldName = "fieldName";

let keyValueGadtPackedName = "Field";

let variantNameFromfieldName = fieldName => String.capitalize(fieldName);

let fieldNameGadt = (~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_type(
      Recursive,
      [
        {
          ptype_loc: Location.none,
          ptype_attributes: [],
          ptype_name: {
            txt: gadtFieldName,
            loc: Location.none,
          },
          ptype_params: [
            (
              {ptyp_desc: Ptyp_any, ptyp_loc: Location.none, ptyp_attributes: []},
              Invariant,
            ),
          ],
          ptype_cstrs: [],
          ptype_kind:
            Ptype_variant(
              List.map(
                field =>
                  {
                    pcd_loc: Location.none,
                    pcd_attributes: [],
                    pcd_name: {
                      txt: variantNameFromfieldName(field.pld_name.txt),
                      loc: Location.none,
                    },
                    pcd_args: Pcstr_tuple([]),
                    pcd_res:
                      Some({
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_constr(
                            {txt: Lident(gadtFieldName), loc: Location.none},
                            [
                              {
                                ptyp_desc: field.pld_type.ptyp_desc,
                                ptyp_loc: Location.none,
                                ptyp_attributes: [],
                              },
                            ],
                          ),
                      }),
                  },
                fields,
              ),
            ),
          ptype_private: Public,
          ptype_manifest: None,
        },
      ],
    ),
};

let fieldGadt = {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_type(
      Recursive,
      [
        {
          ptype_loc: Location.none,
          ptype_attributes: [],
          ptype_name: {
            txt: "field",
            loc,
          },
          ptype_params: [],
          ptype_cstrs: [],
          ptype_kind:
            Ptype_variant([
              {
                pcd_name: {
                  txt: keyValueGadtPackedName,
                  loc,
                },
                pcd_args:
                  Pcstr_tuple([
                    {
                      ptyp_loc: Location.none,
                      ptyp_attributes: [],
                      ptyp_desc:
                        Ptyp_constr(
                          {loc, txt: Lident("fieldName")},
                          [
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc: Ptyp_var("a"),
                            },
                          ],
                        ),
                    },
                    {
                      ptyp_desc: Ptyp_var("a"),
                      ptyp_loc: Location.none,
                      ptyp_attributes: [],
                    },
                  ]),
                pcd_res:
                  Some({
                    ptyp_loc: Location.none,
                    ptyp_attributes: [],
                    ptyp_desc: Ptyp_constr({loc, txt: Lident("field")}, []),
                  }),
                pcd_loc: Location.none,
                pcd_attributes: [],
              },
            ]),
          ptype_private: Public,
          ptype_manifest: None,
        },
      ],
    ),
};

let set = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_pat: {
            ppat_loc: Location.none,
            ppat_attributes: [],
            ppat_desc:
              Ppat_constraint(
                {
                  ppat_desc: Ppat_var({txt: "set", loc}),
                  ppat_loc: Location.none,
                  ppat_attributes: [],
                },
                {
                  ptyp_loc: Location.none,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_poly(
                      [{txt: "value", loc}],
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_desc: Ptyp_constr({txt: Lident(typeName), loc}, []),
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                            },
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_desc: Ptyp_var("value"),
                                            ptyp_loc: Location.none,
                                            ptyp_attributes: [],
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_arrow(
                                        Nolabel,
                                        {
                                          ptyp_desc: Ptyp_var("value"),
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                        },
                                        {
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident(typeName), loc},
                                              [],
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_newtype(
                {txt: "value", loc},
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_constraint(
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_desc: Ppat_var({txt: typeName, loc}),
                              ppat_loc: Location.none,
                              ppat_attributes: [],
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_fun(
                                  Nolabel,
                                  None,
                                  {
                                    ppat_loc: Location.none,
                                    ppat_attributes: [],
                                    ppat_desc: Ppat_var({txt: gadtFieldName, loc}),
                                  },
                                  {
                                    pexp_loc: loc,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_fun(
                                        Nolabel,
                                        None,
                                        {
                                          ppat_loc: Location.none,
                                          ppat_attributes: [],
                                          ppat_desc: Ppat_var({txt: "value", loc}),
                                        },
                                        {
                                          pexp_loc: loc,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_match(
                                              {
                                                pexp_loc: loc,
                                                pexp_attributes: [],
                                                pexp_desc:
                                                  Pexp_ident({
                                                    txt: Lident(gadtFieldName),
                                                    loc,
                                                  }),
                                              },
                                              List.map(
                                                field =>
                                                  {
                                                    pc_lhs: {
                                                      ppat_loc: Location.none,
                                                      ppat_attributes: [],
                                                      ppat_desc:
                                                        Ppat_construct(
                                                          {
                                                            loc,
                                                            txt:
                                                              Lident(
                                                                String.capitalize(
                                                                  field.pld_name.txt,
                                                                ),
                                                              ),
                                                          },
                                                          None,
                                                        ),
                                                    },
                                                    pc_guard: None,
                                                    pc_rhs: {
                                                      pexp_loc: loc,
                                                      pexp_attributes: [],
                                                      pexp_desc:
                                                        Pexp_record(
                                                          [
                                                            (
                                                              {
                                                                loc,
                                                                txt:
                                                                  Lident(
                                                                    field.pld_name.txt,
                                                                  ),
                                                              },
                                                              {
                                                                pexp_loc: loc,
                                                                pexp_attributes: [],
                                                                pexp_desc:
                                                                  Pexp_ident({
                                                                    loc,
                                                                    txt:
                                                                    Lident("value"),
                                                                  }),
                                                              },
                                                            ),
                                                          ],
                                                          Some({
                                                            pexp_loc: loc,
                                                            pexp_attributes: [],
                                                            pexp_desc:
                                                              Pexp_ident({
                                                                loc,
                                                                txt: Lident(typeName),
                                                              }),
                                                          }),
                                                        ),
                                                    },
                                                  },
                                                fields,
                                              ),
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr({txt: Lident(typeName), loc}, []),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_loc: loc,
                                            ptyp_attributes: [],
                                            ptyp_desc:
                                              Ptyp_constr(
                                                {txt: Lident("value"), loc},
                                                [],
                                              ),
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_arrow(
                                        Nolabel,
                                        {
                                          ptyp_loc: loc,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident("value"), loc},
                                              [],
                                            ),
                                        },
                                        {
                                          ptyp_loc: loc,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident(typeName), loc},
                                              [],
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let get = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_pat: {
            ppat_loc: Location.none,
            ppat_attributes: [],
            ppat_desc:
              Ppat_constraint(
                {
                  ppat_desc: Ppat_var({txt: "get", loc: Location.none}),
                  ppat_loc: Location.none,
                  ppat_attributes: [],
                },
                {
                  ptyp_loc: Location.none,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_poly(
                      [{txt: "value", loc}],
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident(typeName), loc: Location.none},
                                  [],
                                ),
                            },
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {
                                          txt: Lident(gadtFieldName),
                                          loc: Location.none,
                                        },
                                        [
                                          {
                                            ptyp_desc: Ptyp_var("value"),
                                            ptyp_loc: Location.none,
                                            ptyp_attributes: [],
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_desc: Ptyp_var("value"),
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_newtype(
                {txt: "value", loc},
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_constraint(
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_desc: Ppat_var({txt: typeName, loc: Location.none}),
                              ppat_loc: Location.none,
                              ppat_attributes: [],
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_fun(
                                  Nolabel,
                                  None,
                                  {
                                    ppat_desc: Ppat_var({txt: gadtFieldName, loc}),
                                    ppat_loc: loc,
                                    ppat_attributes: [],
                                  },
                                  {
                                    pexp_loc: loc,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_match(
                                        {
                                          pexp_loc: loc,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_ident({
                                              txt: Lident(gadtFieldName),
                                              loc,
                                            }),
                                        },
                                        List.map(
                                          field =>
                                            {
                                              pc_lhs: {
                                                ppat_loc: loc,
                                                ppat_attributes: [],
                                                ppat_desc:
                                                  Ppat_construct(
                                                    {
                                                      loc,
                                                      txt:
                                                        Lident(
                                                          String.capitalize(
                                                            field.pld_name.txt,
                                                          ),
                                                        ),
                                                    },
                                                    None,
                                                  ),
                                              },
                                              pc_guard: None,
                                              pc_rhs: {
                                                pexp_loc: loc,
                                                pexp_attributes: [],
                                                pexp_desc:
                                                  Pexp_field(
                                                    {
                                                      pexp_loc: loc,
                                                      pexp_attributes: [],
                                                      pexp_desc:
                                                        Pexp_ident({
                                                          loc,
                                                          txt: Lident(typeName),
                                                        }),
                                                    },
                                                    {
                                                      loc,
                                                      txt: Lident(field.pld_name.txt),
                                                    },
                                                  ),
                                              },
                                            },
                                          fields,
                                        ),
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr({txt: Lident(typeName), loc}, []),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_loc: loc,
                                            ptyp_attributes: [],
                                            ptyp_desc:
                                              Ptyp_constr(
                                                {txt: Lident("value"), loc},
                                                [],
                                              ),
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr({txt: Lident("value"), loc}, []),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let toInt = (~fields) => {
  pstr_loc: loc,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_pat: {
            ppat_attributes: [],
            ppat_loc: loc,
            ppat_desc:
              Ppat_constraint(
                {
                  ppat_attributes: [],
                  ppat_loc: loc,
                  ppat_desc: Ppat_var({txt: "toInt", loc}),
                },
                {
                  ptyp_attributes: [],
                  ptyp_loc: loc,
                  ptyp_desc:
                    Ptyp_poly(
                      [{txt: "value", loc}],
                      {
                        ptyp_attributes: [],
                        ptyp_loc: loc,
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_attributes: [],
                              ptyp_loc: loc,
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident("fieldName"), loc},
                                  [
                                    {
                                      ptyp_attributes: [],
                                      ptyp_loc: loc,
                                      ptyp_desc: Ptyp_var("value"),
                                    },
                                  ],
                                ),
                            },
                            {
                              ptyp_attributes: [],
                              ptyp_loc: loc,
                              ptyp_desc: Ptyp_constr({txt: Lident("int"), loc}, []),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_newtype(
                {txt: "value", loc},
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_constraint(
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_attributes: [],
                              ppat_loc: loc,
                              ppat_desc: Ppat_var({txt: "fieldName", loc}),
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_match(
                                  {
                                    pexp_attributes: [],
                                    pexp_loc: loc,
                                    pexp_desc:
                                      Pexp_ident({txt: Lident("fieldName"), loc}),
                                  },
                                  List.mapi(
                                    (index, field) =>
                                      {
                                        pc_lhs: {
                                          ppat_loc: loc,
                                          ppat_attributes: [],
                                          ppat_desc:
                                            Ppat_construct(
                                              {
                                                txt:
                                                  Lident(
                                                    variantNameFromfieldName(
                                                      field.pld_name.txt,
                                                    ),
                                                  ),
                                                loc,
                                              },
                                              None,
                                            ),
                                        },
                                        pc_guard: None,
                                        pc_rhs: {
                                          pexp_loc: loc,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_constant(
                                              Pconst_integer(
                                                string_of_int(index),
                                                None,
                                              ),
                                            ),
                                        },
                                      },
                                    fields,
                                  ),
                                ),
                            },
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident("fieldName"), loc},
                                  [
                                    {
                                      ptyp_loc: loc,
                                      ptyp_attributes: [],
                                      ptyp_desc:
                                        Ptyp_constr({txt: Lident("value"), loc}, []),
                                    },
                                  ],
                                ),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc: Ptyp_constr({txt: Lident("int"), loc}, []),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let compare = {
  pstr_loc: loc,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_pat: {
            ppat_loc: loc,
            ppat_attributes: [],
            ppat_desc: Ppat_var({txt: "compare", loc}),
          },
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_constraint(
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_fun(
                      Nolabel,
                      None,
                      {
                        ppat_loc: loc,
                        ppat_attributes: [],
                        ppat_desc: Ppat_var({txt: "fieldA", loc}),
                      },
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_loc: loc,
                              ppat_attributes: [],
                              ppat_desc: Ppat_var({txt: "fieldB", loc}),
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_apply(
                                  {
                                    pexp_loc: loc,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_ident({
                                        loc,
                                        txt: Ldot(Lident("Pervasives"), "compare"),
                                      }),
                                  },
                                  [
                                    (
                                      Nolabel,
                                      {
                                        pexp_loc: loc,
                                        pexp_attributes: [],
                                        pexp_desc:
                                          Pexp_apply(
                                            {
                                              pexp_loc: loc,
                                              pexp_attributes: [],
                                              pexp_desc:
                                                Pexp_ident({
                                                  loc,
                                                  txt: Lident("toInt"),
                                                }),
                                            },
                                            [
                                              (
                                                Nolabel,
                                                {
                                                  pexp_loc: loc,
                                                  pexp_attributes: [],
                                                  pexp_desc:
                                                    Pexp_ident({
                                                      loc,
                                                      txt: Lident("fieldA"),
                                                    }),
                                                },
                                              ),
                                            ],
                                          ),
                                      },
                                    ),
                                    (
                                      Nolabel,
                                      {
                                        pexp_loc: loc,
                                        pexp_attributes: [],
                                        pexp_desc:
                                          Pexp_apply(
                                            {
                                              pexp_loc: loc,
                                              pexp_attributes: [],
                                              pexp_desc:
                                                Pexp_ident({
                                                  loc,
                                                  txt: Lident("toInt"),
                                                }),
                                            },
                                            [
                                              (
                                                Nolabel,
                                                {
                                                  pexp_loc: loc,
                                                  pexp_attributes: [],
                                                  pexp_desc:
                                                    Pexp_ident({
                                                      loc,
                                                      txt: Lident("fieldB"),
                                                    }),
                                                },
                                              ),
                                            ],
                                          ),
                                      },
                                    ),
                                  ],
                                ),
                            },
                          ),
                      },
                    ),
                },
                {
                  ptyp_loc: loc,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_arrow(
                      Nolabel,
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_constr(
                            {txt: Lident("fieldName"), loc},
                            [
                              {
                                ptyp_loc: loc,
                                ptyp_attributes: [],
                                ptyp_desc: Ptyp_var("a"),
                              },
                            ],
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident("fieldName"), loc},
                                  [
                                    {
                                      ptyp_loc: loc,
                                      ptyp_attributes: [],
                                      ptyp_desc: Ptyp_var("b"),
                                    },
                                  ],
                                ),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc: Ptyp_constr({txt: Lident("int"), loc}, []),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let fold = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: Location.none,
          pvb_attributes: [],
          pvb_pat: {
            ppat_desc: Ppat_var({txt: "fold", loc}),
            ppat_loc: loc,
            ppat_attributes: [],
          },
          pvb_expr: {
            pexp_loc: Location.none,
            pexp_attributes: [],
            pexp_desc:
              Pexp_constraint(
                {
                  pexp_loc: Location.none,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_fun(
                      Nolabel,
                      None,
                      {
                        ppat_desc: Ppat_var({txt: typeName, loc}),
                        ppat_loc: Location.none,
                        ppat_attributes: [],
                      },
                      {
                        pexp_loc: Location.none,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Labelled("initial"),
                            None,
                            {
                              ppat_desc: Ppat_var({txt: "initial", loc}),
                              ppat_loc: Location.none,
                              ppat_attributes: [],
                            },
                            {
                              pexp_loc: Location.none,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_fun(
                                  Labelled("f"),
                                  None,
                                  {
                                    ppat_desc: Ppat_var({txt: "f", loc}),
                                    ppat_loc: Location.none,
                                    ppat_attributes: [],
                                  },
                                  {
                                    pexp_loc: Location.none,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_apply(
                                        {
                                          pexp_loc: Location.none,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_ident({
                                              loc,
                                              txt: Ldot(Lident("Array"), "fold_left"),
                                            }),
                                        },
                                        [
                                          (
                                            Nolabel,
                                            {
                                              pexp_loc: Location.none,
                                              pexp_attributes: [],
                                              pexp_desc:
                                                Pexp_fun(
                                                  Nolabel,
                                                  None,
                                                  {
                                                    ppat_loc: Location.none,
                                                    ppat_attributes: [],
                                                    ppat_desc:
                                                      Ppat_var({loc, txt: "result"}),
                                                  },
                                                  {
                                                    pexp_loc: Location.none,
                                                    pexp_attributes: [],
                                                    pexp_desc:
                                                      Pexp_fun(
                                                        Nolabel,
                                                        None,
                                                        {
                                                          ppat_loc: Location.none,
                                                          ppat_attributes: [],
                                                          ppat_desc:
                                                            Ppat_var({
                                                              loc,
                                                              txt: "field",
                                                            }),
                                                        },
                                                        {
                                                          pexp_loc: Location.none,
                                                          pexp_attributes: [],
                                                          pexp_desc:
                                                            Pexp_apply(
                                                              {
                                                                pexp_loc: Location.none,
                                                                pexp_attributes: [],
                                                                pexp_desc:
                                                                  Pexp_ident({
                                                                    loc,
                                                                    txt: Lident("f"),
                                                                  }),
                                                              },
                                                              [
                                                                (
                                                                  Nolabel,
                                                                  {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_ident({
                                                                    loc,
                                                                    txt:
                                                                    Lident("result"),
                                                                    }),
                                                                  },
                                                                ),
                                                                (
                                                                  Nolabel,
                                                                  {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_ident({
                                                                    loc,
                                                                    txt:
                                                                    Lident("field"),
                                                                    }),
                                                                  },
                                                                ),
                                                              ],
                                                            ),
                                                        },
                                                      ),
                                                  },
                                                ),
                                            },
                                          ),
                                          (
                                            Nolabel,
                                            {
                                              pexp_loc: Location.none,
                                              pexp_attributes: [],
                                              pexp_desc:
                                                Pexp_ident({
                                                  loc,
                                                  txt: Lident("initial"),
                                                }),
                                            },
                                          ),
                                          (
                                            Nolabel,
                                            {
                                              pexp_loc: Location.none,
                                              pexp_attributes: [],
                                              pexp_desc:
                                                Pexp_array(
                                                  List.map(
                                                    field =>
                                                      {
                                                        pexp_loc: Location.none,
                                                        pexp_attributes: [],
                                                        pexp_desc:
                                                          Pexp_construct(
                                                            {
                                                              loc,
                                                              txt:
                                                                Lident(
                                                                  keyValueGadtPackedName,
                                                                ),
                                                            },
                                                            Some({
                                                              pexp_loc: Location.none,
                                                              pexp_attributes: [],
                                                              pexp_desc:
                                                                Pexp_tuple([
                                                                  {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_construct(
                                                                    {
                                                                    loc,
                                                                    txt:
                                                                    Lident(
                                                                    variantNameFromfieldName(
                                                                    field.pld_name.txt,
                                                                    ),
                                                                    ),
                                                                    },
                                                                    None,
                                                                    ),
                                                                  },
                                                                  {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_apply(
                                                                    {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_ident({
                                                                    loc,
                                                                    txt: Lident("get"),
                                                                    }),
                                                                    },
                                                                    [
                                                                    (
                                                                    Labelled(""),
                                                                    {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_ident({
                                                                    loc,
                                                                    txt:
                                                                    Lident(typeName),
                                                                    }),
                                                                    },
                                                                    ),
                                                                    (
                                                                    Labelled(""),
                                                                    {
                                                                    pexp_loc: Location.none,
                                                                    pexp_attributes: [],
                                                                    pexp_desc:
                                                                    Pexp_construct(
                                                                    {
                                                                    loc,
                                                                    txt:
                                                                    Lident(
                                                                    variantNameFromfieldName(
                                                                    field.pld_name.txt,
                                                                    ),
                                                                    ),
                                                                    },
                                                                    None,
                                                                    ),
                                                                    },
                                                                    ),
                                                                    ],
                                                                    ),
                                                                  },
                                                                ]),
                                                            }),
                                                          ),
                                                      },
                                                    fields,
                                                  ),
                                                ),
                                            },
                                          ),
                                        ],
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
                {
                  ptyp_loc: Location.none,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_arrow(
                      Nolabel,
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc: Ptyp_constr({loc, txt: Lident(typeName)}, []),
                      },
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Labelled("initial"),
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc: Ptyp_var("result"),
                            },
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Labelled("f"),
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_arrow(
                                        Nolabel,
                                        {
                                          ptyp_desc: Ptyp_var("result"),
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                        },
                                        {
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_arrow(
                                              Nolabel,
                                              {
                                                ptyp_loc: Location.none,
                                                ptyp_attributes: [],
                                                ptyp_desc:
                                                  Ptyp_constr(
                                                    {txt: Lident("field"), loc},
                                                    [],
                                                  ),
                                              },
                                              {
                                                ptyp_loc: Location.none,
                                                ptyp_attributes: [],
                                                ptyp_desc: Ptyp_var("result"),
                                              },
                                            ),
                                        },
                                      ),
                                  },
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc: Ptyp_var("result"),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let fieldsModule = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_module({
      pmb_name: {
        txt: "Fields",
        loc,
      },
      pmb_attributes: [],
      pmb_loc: Location.none,
      pmb_expr: {
        pmod_loc: Location.none,
        pmod_attributes: [],
        pmod_desc:
          Pmod_structure([
            fieldNameGadt(~fields),
            fieldGadt,
            get(~typeName, ~fields),
            set(~typeName, ~fields),
            toInt(~fields),
            compare,
            fold(~typeName, ~fields),
          ]),
      },
    }),
};

let createModule = (~moduleLocation, ~typeDef, ~typeName, ~fields) =>
  Mod.mk(
    ~loc=moduleLocation,
    Pmod_structure([
      typeDef,
      fieldsModule(~typeName, ~fields),
    ]),
  );

let fieldsMapper = (_, _) => {
  ...default_mapper,
  module_expr: (mapper, expr) =>
    switch (expr) {
    | {
        pmod_attributes,
        pmod_loc: moduleLocation,
        pmod_desc:
          Pmod_structure([
            {
              pstr_loc: pstrLoc,
              pstr_desc:
                Pstr_type(
                  rec_flag,
                  [
                    {
                      ptype_name: typeName,
                      ptype_kind: Ptype_record(fields),
                      ptype_params: [],
                      ptype_cstrs: [],
                      ptype_private: Public,
                      ptype_manifest: None,
                      ptype_loc: typeLocation,
                      ptype_attributes: [
                        (
                          {txt: "deriving", loc: _},
                          PStr([
                            {
                              pstr_desc:
                                Pstr_eval(
                                  {
                                    pexp_loc: _,
                                    pexp_desc:
                                      Pexp_ident({txt: Lident("fields"), loc: _}),
                                    _,
                                  },
                                  [],
                                ),
                              _,
                            },
                          ]),
                        ),
                      ],
                    },
                  ],
                ),
              _, // TODO
            },
          ]),
      } =>
      createModule(
        ~moduleLocation,
        ~typeName=typeName.txt,
        ~fields,
        ~typeDef={
          pstr_loc: pstrLoc,
          pstr_desc:
            Pstr_type(
              rec_flag,
              [
                {
                  ptype_name: typeName,
                  ptype_kind: Ptype_record(fields),
                  ptype_params: [],
                  ptype_cstrs: [],
                  ptype_private: Public,
                  ptype_manifest: None,
                  ptype_attributes: [],
                  ptype_loc: typeLocation,
                },
              ],
            ),
        },
      )
    | _ => default_mapper.module_expr(mapper, expr)
    },
};

Driver.register(~name="ppx_fields", ~args=[], (module OCaml_406), fieldsMapper);