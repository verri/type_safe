// Copyright (C) 2016 Jonathan Müller <jonathanmueller.dev@gmail.com>
// This file is subject to the license terms in the LICENSE file
// found in the top-level directory of this distribution.

#ifndef TYPE_SAFE_HAS_DEFAULT_CONSTRUCTOR_HPP_INCLUDED
#define TYPE_SAFE_HAS_DEFAULT_CONSTRUCTOR_HPP_INCLUDED

namespace type_safe
{
    namespace detail
    {
        template <bool>
        struct has_default_constructor
        {
            constexpr has_default_constructor() = delete;
            constexpr has_default_constructor(int)
            {
            }
        };

        template <>
        struct has_default_constructor<true>
        {
            constexpr has_default_constructor() = default;
            constexpr has_default_constructor(int)
            {
            }
        };
    }
}

#endif // TYPE_SAFE_HAS_DEFAULT_CONSTRUCTOR_HPP_INCLUDED
