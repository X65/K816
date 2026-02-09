use k816_golden_tests::harness::run_fixture;

#[test]
fn minimal_fixture() {
    run_fixture("minimal").expect("fixture should compile and match expected outputs");
}
