use k816_golden_tests::harness::run_fixture;

#[test]
fn minimal_fixture() {
    run_fixture("minimal").expect("fixture should compile and match expected outputs");
}

#[test]
fn vars_basic_fixture() {
    run_fixture("vars-basic").expect("fixture should compile and match expected outputs");
}

#[test]
fn vars_array_fixture() {
    run_fixture("vars-array").expect("fixture should compile and match expected outputs");
}
