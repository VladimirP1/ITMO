#include <string>
#include <exception>

class bad_from_string : std::exception {
public:
    bad_from_string(const std::string& message);

    virtual const char* what() const throw();

private:
    std::string message;
};
