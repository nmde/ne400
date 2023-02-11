module Conversions
    implicit none
    private

    public::kPa_to_psia,C_to_K

contains
    function kPa_to_psia(kPa) result(psia)
        integer,intent(in)::kPa
        real::psia

        psia = 0.145038 * kPa
        write(*,"(I0.3,A,F5.1,A)") kPa, " kPa = ", psia, " psia"
    end function kPa_to_psia

    function C_to_K(C) result(K)
        integer,intent(in)::C
        real::K

        K = 273.15 + C
        write(*,"(I0.3,A,F5.1,A)") C, " C = ", K, " K"
    end function C_to_K

    function F_to_R(F) result(R)
        real,intent(in)::F
        real::R

        R = F + 459.67
        write(*,"(F5.1,A,F5.1,A)") F, " F = ", R, " R"
    end function F_to_R
end module Conversions
