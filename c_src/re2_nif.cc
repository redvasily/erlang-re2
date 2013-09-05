#include <string.h>

#include <string>
#include <map>
#include <algorithm>
#include <iostream>
#include <exception>

#include "erl_nif.h"

#include <re2/re2.h>


using std::string;
using std::exception;
using std::map;


typedef map<string, RE2*> patterns_t;
patterns_t prepared_patterns;


class BadArg : public exception {};

string read_iolist_arg(ErlNifEnv *env, const ERL_NIF_TERM &term) {
  ErlNifBinary bin;
  if (!enif_inspect_iolist_as_binary(env, term, &bin)) {
    throw BadArg();
  }
  string s((char*)bin.data, bin.size);
  return s;
}

string read_atom_arg(ErlNifEnv *env, const ERL_NIF_TERM &term) {
  unsigned int atom_length;
  if (!enif_get_atom_length(env, term, &atom_length, ERL_NIF_LATIN1)) {
    throw BadArg();
  }
  char atom_data[atom_length + 1];
  enif_get_atom(env, term, atom_data, atom_length + 1, ERL_NIF_LATIN1);
  return string(atom_data);
}


static ERL_NIF_TERM full_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  string subj;
  try {
    subj = read_iolist_arg(env, argv[0]);
  } catch (const BadArg &e) {
    return enif_make_badarg(env);
  }

  string key;
  RE2 *pattern;
  try {
    key = read_atom_arg(env, argv[1]);
    pattern = prepared_patterns[key];
  } catch (const BadArg &e) {
    string expression = read_iolist_arg(env, argv[1]);
    pattern = new RE2(expression);
  }

  bool result = RE2::FullMatch(subj, *pattern);

  if (!key.length()) {
    delete pattern;
  }

  return enif_make_atom(env, result ? "true" : "false");
}

static ERL_NIF_TERM prepare(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  string pattern, key;
  try {
    pattern = read_iolist_arg(env, argv[0]);
    key = read_atom_arg(env, argv[1]);
  } catch (const BadArg &e) {
    return enif_make_badarg(env);
  }

  RE2 *re = new RE2(pattern);
  prepared_patterns[key] = re;
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_nr_prepared(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_int(env, prepared_patterns.size());
}

static ERL_NIF_TERM remove_prepared(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  string key;
  try {
    key = read_atom_arg(env, argv[0]);
  } catch (const BadArg &e) {
    return enif_make_badarg(env);
  }

  patterns_t::iterator it = prepared_patterns.find(key);
  if (it == prepared_patterns.end()) {
    return enif_make_atom(env, "error");
  }
  delete it->second;
  prepared_patterns.erase(it);
  return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"full_match", 2, full_match},
    {"prepare", 2, prepare},
    {"get_nr_prepared", 0, get_nr_prepared},
    {"remove_prepared", 1, remove_prepared}
};

ERL_NIF_INIT(re2, nif_funcs, NULL, NULL, NULL, NULL)
