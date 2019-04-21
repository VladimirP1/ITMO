#include "BadFromString.h"

bad_from_string::bad_from_string(const std::string &message) : message(message) {

}

const char *bad_from_string::what() const throw() {
    return message.c_str();
}
