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

    // Alla stringa da stampare aggiunge i commenti, se presenti e incrementa il numero di linea
    void add_comments_to_string_if_any(std::string& pretty_printer, const std::map<int, std::string> comments, int& lin_num) {
        auto it = comments.find(lin_num);
        while (it != comments.end()) {
            pretty_printer += it->second;
            it = comments.find(++lin_num);
        }
    }

public:
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

    // Aggiunge tutti i puntatori in avanti e in indietro ai corretti riferimenti
    // ERROR: se il puntatore punta a una sezione che non esiste
    // ERROR: se il puntatore punta a un assegnamento che non esiste, nella corretta sezione
    void add_pointers_references() {
        for (auto& section : sections) {
            for(auto& assignment : section.second) {
                auto& ass = assignment.second.first;
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
                    auto& pointed_section = sections[sec_name_pointed];

                    // get pointed variable
                    auto temp_var = pointed_section.find(p_name);
                    if (temp_var == pointed_section.end()) {
                        throw std::runtime_error("Found a pointer to variable \"" + p_name + "\" in section \"" + sec_name_pointed + "\" but variable doesn't exist");
                    }
                    auto& pointed_assig = pointed_section[p_name];

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

    // Dato un asignment, cancellare lui e gesitre tutti i riferimenti
    // Caso1: A -> B -> C, cancello B, aggiorno puntatore di A da B a C, nella backlist di C metto A al posto di B
    // Caso2: A -> B -> C, cancello C, cambio il Value (che è di tipo Pointer) di B con quello che aveva C
    // Caso3: A -> B -> C, cancello A, cancello nella backlist di B il puntatore ad A
    // Caso4: A, cancello A
    // ERROR: se la sezione dell'assignemnt da eliminare non esiste
    void delete_assignment(std::string section_name, std::string assignment_to_delete){
        auto section = sections.find(section_name);
        if (section == sections.end()) {
            throw std::runtime_error("Section \"" + section_name + "\" not exist");
        }

        auto assignment = section->second.find(assignment_to_delete);
        if (assignment == section->second.end()) {
            throw std::runtime_error("Assignment \"" + assignment_to_delete + "\" not exist in section \"" + section->first + "\" ");
        }

        // Controllo se è un puntatore
        if(assignment->second.first.type() == Value::ContentType::Pointer) {
            std::pair<std::string, std::string> puntato = assignment->second.first.pointerAssignment();
            puntato.first = (puntato.first == "") ? section_name : puntato.first;

            auto temp_section = sections.find(puntato.first);
            auto temp_assignment = temp_section->second.find(puntato.second);

            if(! assignment->second.second.empty()) {
                // Caso1
                // aggiungere alla backlist del puntato il puntatore a quelli nella backlist di quello da eliminare
                for(auto& elemento_da_aggiungere : assignment->second.second) {
                    temp_assignment->second.second.push_back(elemento_da_aggiungere);
                }

                std::pair<std::string, Value*> puntato = assignment->second.first.pointerValue();

                // quelli nella backlist punteranno al puntato e non a quello eliminato
                for (auto& elemento_backlist : assignment->second.second) {
                    (*elemento_backlist).setPointerValue(puntato.first, puntato.second);
                }
            }
            // Caso3
            // eliminare nella backlist del puntato il puntatore a quello che elimino
            temp_assignment->second.second.remove(&(assignment->second.first));
        } else {
            if(! assignment->second.second.empty()) {
                // Caso2
                // trasformare nella backlist i puntatori, facendoli diventare valori con stesso valore attuale dell'eliminato
                for (auto& elemento_backlist : assignment->second.second) {
                    (*elemento_backlist) = Value(assignment->second.first);
                }
            }
            // Caso4
        }
        // eliminare l'assignment da section
        section->second.erase(assignment);
    }

    // Cancellazione di una sezione e riferimenti a quella sezione
    // ERROR: se non è presente una sezione con quel nome
    void delete_section(const std::string name){
        auto section = sections.find(name);
        if (section == sections.end()) {
            throw std::runtime_error("Requesting deletion of section " + name + " but it doesn't exist");
        }

        auto assignments_to_delete = section->second;
        for(auto& assignment : assignments_to_delete) {
            delete_assignment(name, assignment.first);
        }

        // Eliminare la sezione ormai vuota
        sections.erase(section);
    }

    // Restistuisce una stringa contenente il pretty_printer della Configurazione
    // direct_value è "true" se si vogliono associare ai vari assegnamenti i valori delle variabili puntate, "false" per ottenere i riferimenti.
    std::string to_String(bool direct_value){
        std::string pretty_printer = "";

        for(auto& section : sections){
            pretty_printer += "[" + section.first + "]\n";
            for(auto& assignment : section.second) {
                pretty_printer += "\t" + assignment.first + " = " + assignment.second.first.to_String(direct_value) + "\n";
            }
        }

        return pretty_printer;
    }

    // Restistuisce una stringa contenente il pretty_printer della Configurazione con i commenti
    // I commenti devono essere una map di coppie (numero linea, stringa contenente il commento)
    // direct_value è "true" se si vogliono associare ai vari assegnamenti i valori delle variabili puntate, "false" per ottenere i riferimenti.
    std::string to_String(std::map<int, std::string> comments, bool direct_value){
        std::string pretty_printer = "";
        int lin_num = 1;

        add_comments_to_string_if_any(pretty_printer, comments, lin_num);

        for(auto& section : sections){
            pretty_printer += "[" + section.first + "]\n";

            add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);

            for(auto& assignment : section.second) {
                pretty_printer += "\t" + assignment.first + " = " + assignment.second.first.to_String(direct_value) + "\n";
                add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);
            }
        }

        add_comments_to_string_if_any(pretty_printer, comments, ++lin_num);

        return pretty_printer;
    }
};

std::ostream& operator<<(std::ostream &strm, Configuration &s) {
    return strm << s.to_String(false);
}
