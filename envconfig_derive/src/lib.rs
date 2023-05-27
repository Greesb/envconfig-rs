//! Provides a derive macro that implements `Envconfig` trait.
//! For complete documentation please see [envconfig](https://docs.rs/envconfig).

use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parenthesized, Attribute, DeriveInput, Field, Fields, Ident, Lit, LitStr, Meta};

#[proc_macro_derive(Envconfig, attributes(envconfig))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input: DeriveInput = syn::parse(input).unwrap();
    let gen = impl_envconfig(&derive_input);
    gen.into()
}

enum Source {
    Environment,
    HashMap,
}

fn impl_envconfig(input: &DeriveInput) -> proc_macro2::TokenStream {
    use syn::Data::*;
    let struct_name = &input.ident;

    let input_attr = fetch_envconfig_attr_from_attrs(&input.attrs);
    let env_prefix = match input_attr {
        Some(attr) => find_item_in_attr_meta(&input.ident as &Option<proc_macro2::Ident>, &attr, "env_prefix"),
        None => None,
    };

    let inner_impl = match input.data {
        Struct(ref ds) => match ds.fields {
            Fields::Named(ref fields) => impl_envconfig_for_struct(struct_name, &fields.named, env_prefix),
            _ => panic!("envconfig supports only named fields"),
        },
        _ => panic!("envconfig only supports non-tuple structs"),
    };

    quote!(#inner_impl)
}

fn impl_envconfig_for_struct(
    struct_name: &Ident,
    fields: &Punctuated<Field, Comma>,
    env_prefix: Option<LitStr>,
) -> proc_macro2::TokenStream {
    let field_assigns_env = fields
        .iter()
        .map(|field| gen_field_assign(field, Source::Environment, &env_prefix));
    let field_assigns_hashmap = fields
        .iter()
        .map(|field| gen_field_assign(field, Source::HashMap, &env_prefix));

    quote! {
        impl Envconfig for #struct_name {
            fn init_from_env() -> ::std::result::Result<Self, ::envconfig::Error> {
                let config = Self {
                    #(#field_assigns_env,)*
                };
                Ok(config)
            }

            fn init_from_hashmap(hashmap: &::std::collections::HashMap<String, String>) -> ::std::result::Result<Self, ::envconfig::Error> {
                let config = Self {
                    #(#field_assigns_hashmap,)*
                };
                Ok(config)
            }

            fn init() -> ::std::result::Result<Self, ::envconfig::Error> {
                Self::init_from_env()
            }
        }
    }
}

fn gen_field_assign(field: &Field, source: Source, env_prefix: &Option<LitStr>) -> proc_macro2::TokenStream {
    let attr = fetch_envconfig_attr_from_attrs(&field.attrs);
    let env_prefix_str = match env_prefix {
        Some(prefix) => prefix.value(),
        None => "".to_string(),
    };

    if let Some(attr) = attr {
        // if #[envconfig(...)] is there
        let list = fetch_list_from_attr(&field.ident, attr);

        // If nested attribute is present
        let nested_value_opt = find_item_in_attr_meta(&field.ident, &attr, "nested");
        if nested_value_opt.is_some() {
            return gen_field_assign_for_struct_type(field, source);
        }

        let opt_default = find_item_in_attr_meta(&field.ident, &attr, "default");

        let from_opt = find_item_in_attr_meta(&field.ident, &attr, "from");
        let env_var_name: String = match from_opt {
            Some(v) => format!("{}{}", env_prefix_str, v.value()),
            //get_lit_str_value(v, &field.ident)),
            None => field_to_env_var(field, env_prefix_str),
        };
        let env_var = quote! { #env_var_name };

        gen(field, env_var, opt_default, source)
    } else {
        // if #[envconfig(...)] is not present
        let env_var_name = field_to_env_var(field, env_prefix_str);
        let env_var = quote! { #env_var_name };
        gen(field, env_var, None, source)
    }
}

fn get_lit_str_value(lit_obj: &Lit, field_ident: &Option<Ident>) -> String {
    match lit_obj {
        Lit::Str(value) => value.value(),
        _ => panic!(
            "Expected '{}' field option 'from' type to be a String",
            to_s(field_ident)
        ),
    }
}


fn field_to_env_var(field: &Field, env_prefix_str: String) -> String {
    let ident_name = field.clone().ident.unwrap().to_string().to_uppercase();
    format!("{}{}", env_prefix_str, ident_name)
}

fn gen(
    field: &Field,
    from: proc_macro2::TokenStream,
    opt_default: Option<&Lit>,
    source: Source,
) -> proc_macro2::TokenStream {
    let field_type = &field.ty;
    if to_s(field_type).starts_with("Option ") {
        gen_field_assign_for_optional_type(field, from, opt_default, source)
    } else {
        gen_field_assign_for_non_optional_type(field, from, opt_default, source)
    }
}

fn gen_field_assign_for_struct_type(field: &Field, source: Source) -> proc_macro2::TokenStream {
    let ident = &field.ident;
    match &field.ty {
        syn::Type::Path(path) => match source {
            Source::Environment => quote! {
                #ident: #path :: init_from_env()?
            },
            Source::HashMap => quote! {
                #ident: #path :: init_from_hashmap(hashmap)?
            },
        },
        _ => panic!("Expected field type to be a path: {:?}", ident),
    }
}

fn gen_field_assign_for_optional_type(
    field: &Field,
    from: proc_macro2::TokenStream,
    opt_default: Option<&Lit>,
    source: Source,
) -> proc_macro2::TokenStream {
    let field_name = &field.ident;

    if opt_default.is_some() {
        panic!("Optional type on field `{}` with default value does not make sense and therefore is not allowed", to_s(field_name));
    }

    match source {
        Source::Environment => quote! {
            #field_name: ::envconfig::load_optional_var(#from, None)?
        },
        Source::HashMap => quote! {
            #field_name: ::envconfig::load_optional_var(#from, Some(hashmap))?
        },
    }
}

fn gen_field_assign_for_non_optional_type(
    field: &Field,
    from: proc_macro2::TokenStream,
    opt_default: Option<&Lit>,
    source: Source,
) -> proc_macro2::TokenStream {
    let field_name = &field.ident;

    if let Some(default) = opt_default {
        match source {
            Source::Environment => quote! {
                #field_name: ::envconfig::load_var_with_default(#from, None, #default)?
            },
            Source::HashMap => quote! {
                #field_name: ::envconfig::load_var_with_default(#from, Some(hashmap), #default)?
            },
        }
    } else {
        match source {
            Source::Environment => quote! {
                #field_name: ::envconfig::load_var(#from, None)?
            },
            Source::HashMap => quote! {
                #field_name: ::envconfig::load_var(#from, Some(hashmap))?
            },
        }
    }
}

fn fetch_envconfig_attr_from_attrs(attrs: &Vec<Attribute>) -> Option<&Attribute> {
    attrs.iter().find(|a| {
        let path = &a.path();
        let name = quote!(#path).to_string();
        name == "envconfig"
    })
}

fn find_item_in_attr_meta<'n>(attr_owner_ident: &Option<Ident> , attr: &Attribute, item_name: &'n str) -> Option<LitStr> {
    let mut item: Option<LitStr> = None;
    attr.parse_nested_meta(|meta| {
        if meta.path.is_ident(item_name) {
            let content;
            parenthesized!(content in meta.input);
            item = Some(content.parse()?);
            return Ok(());
        }

        Ok(())
    });

    item
}

//fn find_item_in_list<'i, 'l, 'n>(
//    list_owner_ident: &'i Option<Ident>,
//    list: &'l Punctuated<NestedMeta, Comma>,
//    item_name: &'n str,
//) -> Option<&'l Lit> {
//    list.iter()
//        .map(|item| match item {
//            NestedMeta::Meta(meta) => match meta {
//                Meta::NameValue(name_value) => name_value,
//                _ => panic!(
//                    "`envconfig` attribute on `{}` must contain name/value item",
//                    to_s(&list_owner_ident),
//                ),
//            },
//            _ => panic!(
//                "Failed to process `envconfig` attribute on `{}`",
//                to_s(&list_owner_ident),
//            ),
//        })
//        .find(|name_value| name_value.path.is_ident(item_name))
//        .map(|item| &item.lit)
//}

fn to_s<T: quote::ToTokens>(node: &T) -> String {
    quote!(#node).to_string()
}
