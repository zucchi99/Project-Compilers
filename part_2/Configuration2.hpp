#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <list>
#include <algorithm>
#include <memory>

#include "Value.hpp"

// Si è pensato ad una struttura dati Map, avente:
//  - Nome sezione come key, string
//  - Lista di assegnazioni come value, Map avente:
//      - Label come key, string
//      - coppia:
//          - Value come value, classe Value con 4 possibiltà (bool, string, int, coppia <stringa, puntatore a Value>)
//          - Lista di puntatori all'indietro

class Configuration {
private:

    std::map<
        // section name
        std::string, 
        // list of assignments
        std::map<
            // variable name
            std::string, 
            std::pair<
                // value of variable assigned
                Value, 
                // list of entring pointers
                std::list<Value*>
            >
        >
    > sections;

    // Data una lista di coppie <sezione, label> cancella ricorsivamente tutte le variabili associate
    /* void delete_assignments_of_section(std::list<std::pair<std::string, std::string>> assignments_to_delete){
        if(!assignments_to_delete.empty()) {
            // Prendo il primo assegnamento dalla lista, lo elimino da essa e genero la stringa refpath
            auto section_assignment = assignments_to_delete.front();
            assignments_to_delete.pop_front();
            std::string refpath = "$" + section_assignment.first + "." + section_assignment.second;

            // Eliminare l'assegnamento dalla struttua
            sections.find(section_assignment.first)->second.erase(section_assignment.second);

            // Controlla in ogni sezione se c'è una variabile che si riferisce a quella sezione e se la trova la aggiunge alla lista
            for(const auto& section : sections)
                for(const auto& assignment : section.second)
                    if(assignment.second == refpath)
                        assignments_to_delete.emplace_back(section.first, assignment.first);

            // Ripeto la procedura fino a che non finisce la lista
            delete_assignments_of_section(assignments_to_delete);
        }
    } */

    // Alla stringa da stampare aggiunge i commenti, se presenti e incrementa il numero di linea
    void add_comments_to_string_if_any(std::string& pretty_printer, const std::map<int, std::string> comments, int& lin_num) {
        auto it = comments.find(lin_num);
        while (it != comments.end()) {
            pretty_printer += it->second;
            it = comments.find(++lin_num);
        }
    }

public:

    void add_pointers_references() {
        for (auto& section : sections) {
            for(auto& assignment : section.second) {
                auto ass = assignment.second.first;
                if(ass.type() == Value::ContentType::Pointer) {

                    // get current pointer name
                    std::string p_name = ass.pointerValue().first;
                    // get current section name
                    std::string cur_sec_name = section.first;

                    // get section name pointed
                    int dot_idx = p_name.find('.');
                    std::string sec_name_pointed = (dot_idx == std::string::npos) ? cur_sec_name : p_name.substr(1,dot_idx-1);

                    // get variable name pointed
                    if(dot_idx != std::string::npos) {
                        // remove section name pointed
                        p_name = p_name.substr(dot_idx+1);
                    } else {
                        // remove starting $
                        p_name = p_name.substr(1);
                    }

                    // get pointed section                     
                    auto temp_sec = sections.find(sec_name_pointed);
                    if (temp_sec == sections.end()) {
                        throw std::runtime_error("Found a pointer to section \"" + sec_name_pointed + "\" but section doesn't exist");
                    }
                    auto pointed_section = sections[sec_name_pointed];

                    // get pointed variable
                    auto temp_var = pointed_section.find(p_name);
                    if (temp_var == pointed_section.end()) {
                        throw std::runtime_error("Found a pointer to variable \"" + p_name + "\" in section \"" + sec_name_pointed + "\" but variable doesn't exist");
                    }
                    auto pointed_assig = pointed_section[p_name];

                    // get pointer to value of pointed variable
                    Value* pointed_value = &pointed_assig.first;
                    // set exiting pointer
                    ass.setPointerValue(pointed_value);
                    
                    // get pointer to value of pointing variable
                    Value* pointing_value = &ass;
                    // add entring pointer
                    pointed_assig.second.push_back(pointing_value);

                }
            }
        }
    }
    

    // Inserimento di una sezione
    // ERROR: se è già presente una sezione con quel nome
    void insert_empty_section(const std::string name){
        auto section = sections.find(name);
        if (! (section == sections.end()) ) {
            throw std::runtime_error("Section \"" + name + "\" already defined");
        }
        sections.emplace(name, std::map<std::string, std::pair<Value, std::list<Value*>>>());
    }

    // Append di assignments ad 1 sezione
    // ERROR: se la sezione non esiste
    // WARNING: se uno o più assegnamenti vengono sovrascritti
    // RETURN: true, se non ci sono sovrascritture; false, se ci sono sovrascritture 
    bool modify_assignments_to_section(const std::string name, const std::map<std::string, std::pair<Value, std::list<Value*>>> assignments){
        auto section = sections.find(name);
        if (section == sections.end()) {
            throw std::runtime_error("Adding assignments to section \"" + name + "\" but it doesn't exist");
        }

        bool overwrites = false;

        for(auto& assignment : assignments) {
            auto ret = section->second.insert(assignment);
            if (ret.second == false) {
                std::cout << "WARNING: In the section \"" + name + "\" the assignment \"" + assignment.first + "\" already existed" << "\n\n";
                section->second.at(assignment.first) = assignment.second;
                overwrites = true;
            }
        }

        return !overwrites;
    }

    // Cancellazione di una sezione e riferimenti a quella sezione
    // ERROR: se non è presente una sezione con quel nome
    /* void delete_section(const std::string name){
        auto section = sections.find(name);
        if (section == sections.end()) {
            throw std::runtime_error("Requesting deletion of section " + name + " but it doesn't exist");
        }

        // Per ogni assignment della sezione, elimare lui e ogni suo riferimento
        std::list<std::pair<std::string, std::string>> assignments_to_delete;
        for(auto& assignment : section->second) {
            assignments_to_delete.emplace_back(section->first, assignment.first);
        }
        delete_assignments_of_section(assignments_to_delete);

        // Eliminare la sezione ormai vuota
        sections.erase(section);
    } */

    // Restistuisce una stringa contenente il pretty_printer della Configurazione
    std::string to_String(){
        std::string pretty_printer = "";

        for(auto& section : sections){
            pretty_printer += section.first + "{\n";
            for(auto& assignment : section.second) {
                pretty_printer += "\t" + assignment.first + " = " + assignment.second.first.to_String() + "\n";
            }
            pretty_printer += "}\n";
        }

        return pretty_printer;
    }

    // Restistuisce una stringa contenente il pretty_printer della Configurazione con i commenti
    // I commenti devono essere una map di coppie (numero linea, stringa contenente il commento)
    std::string to_String_with_comments(std::map<int, std::string> comments){
        std::string pretty_printer = "";
        int lin_num = 1;

        add_comments_to_string_if_any(pretty_printer, comments, lin_num);

        for(auto& section : sections){
            pretty_printer += "[" + section.first + "]\n";

            add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);

            for(auto& assignment : section.second) {
                pretty_printer += "\t" + assignment.first + " = " + assignment.second.first.to_String() + "\n";
                add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);
            }
        }

        add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);

        return pretty_printer;
    }
};

std::ostream& operator<<(std::ostream &strm, Configuration &s) {
    return strm << s.to_String();
}
