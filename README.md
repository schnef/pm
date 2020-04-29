# pm (Policy Machine)

Policy Machine Erlang implementation. **This is experimental code**. See [the policy machine
overview](http://csrc.nist.gov/pm/) for more information or read the
specs from [NISTIR 7987rev. 1](http://nvlpubs.nist.gov/nistpubs/ir/2015/NIST.IR.7987r1.pdf).

This version is used to play with some of the ideas the PM tries to
implement. It is of no practical use as it is now.

Take a look at the unit tests at the bottom of the modules on how to
use the functions.

# Run

## First time

Before running the application, the database must be created. Look for
the executable `pm_init` which may be in the `bin` or
`_build/default/bin` directory.

```
~/pm$ ./_build/default/bin/pm_init 
Delete old schema, if any
Create new schema
```

## Running tests

For the unit tests, a temporary database is created (and removed) in
the test directory to prevent corrupting an existing database. Run the
tests by executing:

```
$ rebar3 eunit
<lots of progress reports>
Finished in 2.372 seconds
358 tests, 0 failures
```

The [policy machine core](https://pm-master.github.io/pm-master/policy-machine-core/)
gives a [bank teller example](https://pm-master.github.io/pm-master/policy-machine-core/#bank-teller), which is implemented as one of the unit tests:

```
tst_policy_machine_core_getting_started() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "Bank Teller example"}),
    {ok, Branch_1_usr_attr} = pm_pap:c_ua_in_pc(#ua{value = "Branch 1 user attribute"}, PC),
    {ok, Branch_1_obj_attr} = pm_pap:c_oa_in_pc(#oa{value = "Branch 1 object attribute"}, PC),
    {ok, Teller} = pm_pap:c_ua_in_ua(#ua{value = "Teller"}, Branch_1_usr_attr),
    {ok, Auditor} = pm_pap:c_ua_in_ua(#ua{value = "Auditor"}, Branch_1_usr_attr),
    {ok, U1} = pm_pap:c_u_in_ua(#u{value = "User u1"}, Teller),
    {ok, U2} = pm_pap:c_u_in_ua(#u{value = "User u2"}, Auditor),
    {ok, Accounts} = pm_pap:c_oa_in_oa(#oa{value = "accounts"}, Branch_1_obj_attr),
    {ok, O1} = pm_pap:c_o_in_oa(#o{value = "Account o1"}, Accounts),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    pm_pap:c_assoc(Teller, [AR_r, AR_w], Accounts),
    pm_pap:c_assoc(Auditor, [AR_r], Accounts),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1))].
```
