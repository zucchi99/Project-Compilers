#include <iostream>
#include "Configuration.hpp"

int main() {
    Configuration s;
    std::map<std::string, std::string> assignments = {{ "a", "1" }, { "b", "2" }, { "c", "3" }};
    
    bool a = s.insert_empty_section("prova1");
    
    s.insert_empty_section("prova2");

    s.modify_assignments_to_section("prova1", assignments);

    assignments = { { "c", "1" }, { "d", "2" }, { "e", "3" } };

    s.modify_assignments_to_section("prova1", assignments);

    assignments = { { "c", "1" }, { "d", "2" }, { "e", "$prova1.c" } };

    s.modify_assignments_to_section("prova2", assignments);

    s.delete_section("prova1");

    std::cout << s;

    return 0;
}
