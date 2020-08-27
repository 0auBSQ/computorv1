NAME = computorv1

all: $(NAME)

$(NAME):
	gfortran -Wall -Wextra -Werror -Wno-tabs -Wno-compare-reals computorv1.f90 -o $(NAME)

clean:
	rm -f $(NAME)

fclean: clean

re: fclean all

.PHONY: all clean fclean re