#include <iostream>
#include "Configuration2.hpp"

int main() {
    Configuration s;
    std::map<std::string, Value> assignments = {{ "a", Value(1) }, { "b", Value("2") }, { "c", Value("3") }};
    
    s.insert_empty_section("prova1");
    
    s.insert_empty_section("prova2");

    s.modify_assignments_to_section("prova1", assignments);

    assignments = { { "c",  Value("1") }, { "d",  Value("2") }, { "e",  Value(3) } };

    s.modify_assignments_to_section("prova1", assignments);

    assignments = { { "c",  Value("1") }, { "d",  Value("2") }, { "e",  Value("$prova1.c", NULL) } };

    s.modify_assignments_to_section("prova2", assignments);

    // s.delete_section("prova1");

    std::cout << s;

    return 0;
}
